{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hardware.Intel8080.Model where

import Hardware.Intel8080
import Hardware.Intel8080.Decode
import Hardware.Intel8080.Microcode
import qualified Hardware.Intel8080.MicroCPU as MCPU

import Clash.Prelude
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Control.Monad.Extra (whenM)
import Control.Lens hiding (index)

import Debug.Trace
import Text.Printf

data S = MkS
    { _pc :: Addr
    , _sp :: Addr
    , _allowInterrupts :: Bool
    , _registers :: Vec 8 Value
    , _ureg1 :: Value
    , _ureg2 :: Addr
    }
    deriving (Show)
makeLenses ''S

mkS :: S
mkS = MkS{..}
  where
    _pc = 0
    _sp = 0
    _allowInterrupts = False
    _registers = replace 1 0x02 $ pure 0x00
    _ureg1 = 0
    _ureg2 = 0

data R = MkR
    { readMem :: Addr -> IO Value
    , writeMem :: Addr -> Value -> IO ()
    , inPort :: Port -> IO Value
    , outPort :: Port -> Value -> IO Value
    }

type CPU = MaybeT (RWST R () S IO)

instance MCPU.MicroState S where
    reg r = registers . lens (!! r) (\s v -> replace r v s)
    pc = pc
    sp = sp
    valueBuf = ureg1
    addrBuf = ureg2

instance MCPU.MicroM S CPU where
    loadIn = return ()
    nextInstr = mzero
    allowInterrupts = assign allowInterrupts

dumpState :: CPU ()
dumpState = do
    pc <- use pc
    sp <- use sp
    [bc, de, hl, af] <- mapM (use . MCPU.regPair . uncurry Regs) [(rB, rC), (rD, rE), (rH, rL), (rA, rFlags)]
    liftIO $ do
        printf "IR:         PC: 0x%04x  SP: 0x%04x\n" pc sp
        printf "BC: 0x%04x  DE: 0x%04x  HL: 0x%04x  AF: 0x%04x\n" bc de hl af

fetchByte :: CPU Value
fetchByte = do
    pc <- use pc <* (pc += 1)
    peekByte pc

peekByte :: Addr -> CPU Value
peekByte addr = do
    readMem <- asks readMem
    liftIO $ readMem addr

step :: CPU ()
step = do
    instr <- decodeInstr <$> fetchByte
    exec instr

interrupt :: Instr -> CPU ()
interrupt instr = whenM (use allowInterrupts) $ do
    allowInterrupts .= False
    exec instr

exec :: Instr -> CPU ()
exec instr = do
    let (setup, uops) = microcode instr
    traverse_ (addressing . Left) setup
    -- liftIO $ print (instr, uops)
    mapM_ ustep uops

addressing :: Either Addressing Addressing -> CPU ()
addressing = either (doRead <=< MCPU.targetAddress) (doWrite <=< MCPU.targetAddress)

doWrite :: Either Port Addr -> CPU ()
doWrite target = either writePort poke target =<< use ureg1
  where
    writePort port value = do
        write <- asks outPort
        liftIO $ void $ write port value

    poke addr value = do
        writeMem <- asks writeMem
        liftIO $ writeMem addr value

doRead :: Either Port Addr -> CPU ()
doRead target = assign ureg1 =<< either readPort peekByte target
  where
    readPort port = do
        read <- asks inPort
        liftIO $ read port

ustep :: MicroOp -> CPU ()
ustep (effect, post) = do
    MCPU.uexec effect
    traverse_ addressing post
