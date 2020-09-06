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
    , _targetPort :: Bool
    , _addr :: Addr
    , _write :: Maybe Value
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
    _targetPort = False
    _addr = 0
    _write = Nothing

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
    writeOut = assign write . Just
    readIn = readByte
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

peekByte :: Addr -> CPU Value
peekByte addr = do
    readMem <- asks readMem
    liftIO $ readMem addr

readByte :: CPU Value
readByte = do
    isPort <- use targetPort
    addr <- use addr
    let port = fromIntegral addr
    if isPort then readPort port else peekByte addr

writeByte :: Value -> CPU ()
writeByte x = do
    isPort <- use targetPort
    addr <- use addr
    let port = fromIntegral addr
    if isPort then writePort port x else pokeByte addr x

pokeByte :: Addr -> Value -> CPU ()
pokeByte addr x = do
    writeMem <- asks writeMem
    liftIO $ writeMem addr x

fetchByte :: CPU Value
fetchByte = do
    pc <- use pc <* (pc += 1)
    peekByte pc

writePort :: Port -> Value -> CPU ()
writePort port value = do
    write <- asks outPort
    liftIO $ void $ write port value

readPort :: Port -> CPU Value
readPort port = do
    read <- asks inPort
    liftIO $ read port

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
    uinit
    let (setup, uops) = microcode instr
    traverse_ addressing setup
    -- liftIO $ print (instr, uops)
    mapM_ ustep uops

uinit :: CPU ()
uinit = do
    targetPort .= False

addressing :: Addressing -> CPU ()
addressing Port = do
    (port, _) <- MCPU.twist <$> use ureg2
    tellPort port
addressing Indirect = assign addr =<< use ureg2
addressing IncrPC = assign addr =<< use pc <* (pc += 1)
addressing IncrSP = assign addr =<< use sp <* (sp += 1)
addressing DecrSP = assign addr =<< (sp -= 1) *> use sp

tellPort :: Value -> CPU ()
tellPort port = do
    targetPort .= True
    addr .= bitCoerce (port, port)

ustep :: MicroOp -> CPU ()
ustep (effect, post) = do
    write .= Nothing
    MCPU.uexec effect
    traverse_ addressing post
    traverse_ writeByte =<< use write
