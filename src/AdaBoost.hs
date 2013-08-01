module AdaBoost (adaBoost, Learner) where


type Predictor a b = a -> b
type Learner a b = [a] -> Weights -> Predictor a b
type Weights = [Float]

initialWeights :: [a] -> Weights
initialWeights ex = replicate (length ex) (1/ (fromIntegral (length ex)))

weightedMajority :: [Predictor a b] -> Weights -> Predictor a b
weightedMajority h z = undefined

label :: a -> b
label example = undefined


err :: [Float] -> [Bool] -> Float
err weights predicts = sum (falseWeights weights predicts)

falseIndices :: [Bool] -> [Int]
falseIndices predicts = filter ( \i -> not (predicts !! i) ) [0..(length predicts - 1)]

falseWeights :: [Float] -> [Bool] -> [Float]
falseWeights weights predicts = map (\i -> (weights !! i)) (falseIndices predicts)


correctPredict :: (Eq b) => Predictor a b -> a -> Bool
correctPredict h example = (h example ) == (label example)

predictions :: Eq b => Predictor a b -> [a] -> [Bool]
predictions h examples = map (correctPredict h) examples

updateWeights :: Predictor a b -> [a] -> [Float] -> [Bool] -> Float -> [Float]
updateWeights h examples w p err = map (updW w p err) $ enumFromTo 0 ((length examples) -1)
                                 where 
                                     updW :: [Float] -> [Bool] -> Float -> Int -> Float
                                     updW w p err i = if (p !! i) then ((w !! i) * (err/(1-err))) else (w !! i)
                                     

adaLoop :: Eq b => [a] -> Learner a b -> [Float] -> [Predictor a b] -> [Float] -> Int -> Predictor a b
adaLoop examples l w h z 0 = weightedMajority (reverse h) (reverse z)
adaLoop examples l w hypos z k = let h = (l examples w)
                                     p = predictions h examples    
                                     nH = h : hypos
                                     e = err w p    
                                     updW = updateWeights h examples w p e
                                     nW = map ( \w -> w / (sum updW)) updW
                                     nZ = log ((1 - e) / e) : z
                                     in adaLoop examples l nW nH nZ (k-1)

adaBoost :: Eq b => [a] -> Learner a b -> Int -> Predictor a b
adaBoost examples l k = adaLoop examples l (initialWeights examples) [] [] k
