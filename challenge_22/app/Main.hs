-- Simple Life Calculator
-- Create a program that has 3 simple calculators within it, e.g. VAT, Tax and Times table.  Allow users to choose which calculator they want to use and then carry out that calculation.

import Control.Monad (forM_)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

data Calculation = Vat | Tax | TimesTable deriving (Show)

data TaxBand = TaxBand
        { rate :: Double
        , lowerBound :: Double
        , upperBound :: Maybe Double
        }

bands :: [TaxBand]
bands =
        [ TaxBand{rate = 1, lowerBound = 0, upperBound = Just 12571}
        , TaxBand{rate = 1.2, lowerBound = 12571, upperBound = Just 50271}
        , TaxBand{rate = 1.4, lowerBound = 50271, upperBound = Just 125141}
        , TaxBand{rate = 1.45, lowerBound = 12571, upperBound = Nothing}
        ]

tax :: Double -> Double
tax income =
        sum $
                fmap
                        ( \band ->
                                let taxable = max 0 $ income - lowerBound band
                                 in let taxable' = case upperBound band of
                                                Just upperBound' | upperBound' < income -> max 0 $ taxable - upperBound'
                                                _ -> taxable
                                     in taxable' * rate band
                        )
                        bands

readSatisfies :: (Read a) => String -> (a -> Bool) -> IO a
readSatisfies prompt condition = do
        putStr prompt
        hFlush stdout
        index <- getLine
        case reads index of
                [(index', "")] | condition index' -> return index'
                _ -> do
                        printf "Invalid: %s\n" index
                        readSatisfies prompt condition

readPrompt :: (Read a) => String -> IO a
readPrompt prompt = readSatisfies prompt (const True)

choose :: (Show a) => [a] -> IO a
choose options = do
        forM_
                (zip [1 ..] options)
                ( \(index, option) ->
                        printf
                                "%i. %s\n"
                                (index :: Int)
                                (show option)
                )
        index <- readSatisfies "> " (\index -> index > 0 && index <= length options)
        return $ options !! (index - 1)

main :: IO ()
main = do
        calculation <- choose [Vat, Tax, TimesTable]
        case calculation of
                Vat -> do
                        price <- readPrompt "Price: " :: IO Double
                        printf "%f\n" $ price * 1.2
                Tax -> do
                        income <- readPrompt "Income: "
                        printf "%f\n" $ tax income
                TimesTable -> do
                        timesTable <- readPrompt "Times table: " :: IO Integer
                        forM_ [1 .. 12] (\x -> printf "%i * %i = %i\n" x timesTable (timesTable * x))
