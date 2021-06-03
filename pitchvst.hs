-- This program is intentionally poorly documented; fixing the
-- documentation and actually creating the program are left as exercises
-- for the reader.

import qualified Data.List as DL;
import qualified Data.Function as DF;
import qualified Numeric.FFT as FT;
import Data.Complex;

------------------------------------------------------------------------

windowSize :: Int;
windowSize = 65536;

------------------------------------------------------------------------

type Giraffe = [(Int, Pitch)];  -- LIST OF GRAPH COORDINATES
type Pitch = Double;
type Sample = Double;
type Frequency = Double;
type Multiplier = Double;
type FFTOutput = [Complex Double];
data X11Winda = ExerciseForTheReader;
data WaveType = Sin | Cos;

------------------------------------------------------------------------

-- | createNewWinda creates a new X11 window and returns the tag of this
-- X11 window.
createNewWinda :: IO X11Winda;
createNewWinda = return ExerciseForTheReader;

fft :: [Sample] -> FFTOutput;
fft = FT.fft . map cis;

-- | For all FFT outputs k, fftToPitch equals the loudest pitch of k.
-- Implementing fftToPitch is left as an exercise for the reader.
-- Shut up and hack!
fftToPitch :: FFTOutput -> Pitch;
fftToPitch k = 0;

-- | readMicData is a stream of microphone samples.
-- Implementing readMicData is left as an exercise for the reader. 
-- The author recommends using the SDL library or an operating system-
-- specific approach.
readMicData :: IO [Sample];
readMicData = return $ map (\a -> a * (sin a)) [1.0,1.1..];

-- | For all natural numbers n, readSamp n equals n microphone samples.
readSamp :: Int -> IO [Sample];
readSamp k = readMicData >>= return . take k;


-- | For all [Sample] n, seqToPitch n equals the output of the sequence-
-- to-pitch function on the input n.
seqToPitch :: [Sample] -> Pitch;
seqToPitch = fftToPitch . fft;

-- | genCoords yields the coordinates of the graph.
genCoords :: [Pitch] -> Giraffe;
genCoords x = zip [0..length x - 1] x;

-- | For all X11Winda k, for all Giraffe g, genGraph k g draws
-- g to k, returning the pitch/y values of g.
-- Implementing genGraph is left as an exercise for the reader.
-- The author recommends using Chart or creating a custom library.
genGraph :: X11Winda -> Giraffe -> IO [Pitch];
genGraph w k = putStrLn "TODO: IMPLEMENT GRAPHING." >> return (map snd k);

-- | mane is the main program loop.
mane :: X11Winda -> [Double] -> IO ();
mane w k = readSamp windowSize >>= doTheGraph w >>= loop w
  where
  doTheGraph a b = genGraph a (genCoords $ append k $ seqToPitch b)
  append a b = a ++ [b]
  loop a b = mane a (take 50 b);

-- | main is the program's entry point.  main initialises mane.
main :: IO ();
main = createNewWinda >>= \winda -> mane winda [];
