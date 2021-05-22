windowSize :: Int;
windowSize = 65535;

------------------------------------------------------------------------

type Giraffe = [(Int, Pitch)];
type Pitch = Double;
type Sample = Double;

------------------------------------------------------------------------

readSamp :: Int -> IO [Sample];
readSamp k = return $ take k $ repeat 0;
{- For all natural numbers n, readSamp n equals n samples from the
 - microphone. -}
{- Implementing readSamp is left as an exercise for the reader. -}

seqToPitch :: [Sample] -> Pitch;
seqToPitch _ = 0;
{- For all [Sample] n, seqToPitch n equals the output of the sequence-
 - to-pitch function on the input n. -}
{- Implementing the sequence-to-pitch function is left as an exercise
 - for the reader. -}

calGraph :: [Pitch] -> Giraffe;
calGraph x = zip [0..length x - 1] x;
{- calGraph yields the coordinates of the graph. -}

genGraph :: Giraffe -> IO [Pitch];
genGraph k = putStrLn "TODO: IMPLEMENT GRAPHING." >> return (map snd k);
{- Implementing genGraph is left as an exercise for the reader.
 - The author recommends using Chart or creating a custom library. -}

mane :: [Double] -> IO ();
mane k = readSamp windowSize >>=
  genGraph . calGraph . (:k) . seqToPitch >>= mane;

main :: IO ();
main = mane [];
