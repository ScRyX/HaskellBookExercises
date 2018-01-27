myWords :: String -> [String]
myWords "" = []
myWords w = (takeWhile (/=' ') w):(myWords $ dropWhile (==' ') . dropWhile (/=' ') $ w)

-------------------------------------------

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines l = let notNewLine = (/='\n') in
    (takeWhile notNewLine l):(myLines $ dropWhile (not . notNewLine) . dropWhile notNewLine $ l) 


splitByChar :: Char -> String -> [String]
splitByChar _ "" = []
splitByChar s l = let notSplitter = (/=s) in
    (takeWhile notSplitter l):(splitByChar s $ drop 1 . dropWhile notSplitter $ l) 

-- (splitByChar '\n' sentences) == myLines sentences

