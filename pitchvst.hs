import qualified Data.List as DL;
import qualified Data.Function as DF;
import qualified Numeric.FFT as FT;
import Data.Complex;

------------------------------------------------------------------------

windowSize :: Int;
windowSize = 65536;

------------------------------------------------------------------------

type Giraffe = [(Int, Pitch)];
type Pitch = Double;
type Sample = Double;
type Frequency = Double;
type Multiplier = Double;
type FFTOutput = [Complex Double];
data X11Winda = ExerciseForTheReader;
data WaveType = Sin | Cos;

------------------------------------------------------------------------

createNewWinda :: IO X11Winda;
createNewWinda = return ExerciseForTheReader;
{- createNewWinda creates a new X11 window and returns the tag of this
 - X11 window. -}

fft :: [Sample] -> FFTOutput;
fft = FT.fft . map cis;

fftToPitch :: FFTOutput -> Pitch;
fftToPitch k = 0;
{- For all FFT outputs k, fftToPitch equals the loudest pitch of k. -}
{- Implementing fftToPitch is left as an exercise for the reader.
 - Shut up and hack! -}

readMicData :: IO [Sample];
readMicData = return $ map (\a -> a * (sin a)) [1.0,1.1..];
{- readMicData is a stream of microphone samples. -}
{- Implementing readMicData is left as an exercise for the reader. 
 - The author recommends using the SDL library or an operating system-
 - specific approach. -}

readSamp :: Int -> IO [Sample];
readSamp k = readMicData >>= return . take k;
{- For all natural numbers n, readSamp n equals n microphone samples. -}

seqToPitch :: [Sample] -> Pitch;
seqToPitch = fftToPitch . fft;
{- For all [Sample] n, seqToPitch n equals the output of the sequence-
 - to-pitch function on the input n. -}

genCoords :: [Pitch] -> Giraffe;
genCoords x = zip [0..length x - 1] x;
{- genCoords yields the coordinates of the graph. -}

genGraph :: X11Winda -> Giraffe -> IO [Pitch];
genGraph w k = putStrLn "TODO: IMPLEMENT GRAPHING." >> return (map snd k);
{- Implementing genGraph is left as an exercise for the reader.
 - The author recommends using Chart or creating a custom library. -}

mane :: X11Winda -> [Double] -> IO ();
mane w k = readSamp windowSize >>= doTheGraph w >>= loop w
  where
  doTheGraph a b = genGraph a (genCoords $ append k $ seqToPitch b)
  append a b = a ++ [b]
  loop a b = mane a (take 50 b);

main :: IO ();
main = createNewWinda >>= \winda -> mane winda [];
