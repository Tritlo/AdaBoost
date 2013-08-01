module AdaBoost (adaBoost) where

import Data.List (delete, maximumBy)
import Data.Ord (comparing)

type Predictor a b = a -> b
type Label a b = a -> b
type Learner a b = [a] -> Weights -> Predictor a b
type Weights = [Float]

initialWeights :: [a] -> Weights
initialWeights ex = replicate (length ex) (1/ fromIntegral (length ex))

weightedMajority :: Eq b => [Predictor a b] -> Weights -> Predictor a b
weightedMajority h z ex = fst $ maximumBy (comparing snd) $ (votesWithWeight h z ex)
predicts :: Eq b => [Predictor a b] -> Weights -> a -> [(b,Float)]
predicts h z ex = zip (map (\hy -> hy ex) h) z

votesWithWeight :: Eq b => [Predictor a b] -> Weights -> a -> [(b,Float)]
votesWithWeight h z ex = listToWeightList (predicts h z ex) []

compTuplesByFst a b = compare (fst a) (fst b)
compTuplesBySnd a b = compare (snd a) (snd b)

updateWeightList :: Eq c => (c,Float) -> [(c,Float)] -> [(c,Float)]
updateWeightList (c,w) wL = case lookup c wL of
                                 Nothing -> (c,w) : wL
                                 Just b -> (c, b+w) : (delete (c,b) wL)
                                 
listToWeightList :: Eq a => [(a,Float)] -> [(a,Float)] -> [(a,Float)] 
listToWeightList [] wL = wL
listToWeightList lis wL = listToWeightList (tail lis) (updateWeightList (head lis) wL)


err :: [Float] -> [Bool] -> Float
err weights predicts = sum (falseWeights weights predicts)

falseIndices :: [Bool] -> [Int]
falseIndices predicts = filter ( \i -> not (predicts !! i) ) [0..(length predicts - 1)]

falseWeights :: [Float] -> [Bool] -> [Float]
falseWeights weights predicts = map (\i -> weights !! i) (falseIndices predicts)


correctPredict :: (Eq b) => Predictor a b -> Label a b -> a -> Bool
correctPredict h label example = h example  == label example

predictions :: Eq b => Predictor a b -> [a] ->  Label a b -> [Bool]
predictions h examples label = map (correctPredict label h) examples

updateWeights :: Predictor a b -> [a] -> [Float] -> [Bool] -> Float -> [Float]
updateWeights h examples w p err = map (updW w p err) $ enumFromTo 0 (length examples -1)
                                 where 
                                     updW :: [Float] -> [Bool] -> Float -> Int -> Float
                                     updW w p err i = if p !! i then (w !! i) * (err/(1-err)) else w !! i
                                     

adaLoop :: Eq b => [a] -> Label a b -> Learner a b -> [Float] -> [Predictor a b] -> [Float] -> Int -> Predictor a b
adaLoop examples label l w h z 0 = weightedMajority (reverse h) (reverse z)
adaLoop examples label l w hypos z k = let h = l examples w
                                           p = predictions h examples label 
                                           nH = h : hypos
                                           e = err w p    
                                           updW = updateWeights h examples w p e
                                           nW = map ( \w -> w / sum updW) updW
                                           nZ = log ((1 - e) / e) : z
                                           in adaLoop examples label l nW nH nZ (k-1)

adaBoost :: Eq b => [a] -> Label a b -> Learner a b -> Int -> Predictor a b
adaBoost examples label l k = adaLoop examples label l (initialWeights examples) [] [] k
