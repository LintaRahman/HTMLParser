import Data.Char
import System.IO
import Data.List

main = do
        let file = "file.html"
        contents <- readFile file
        let listOfLabels = tags $ labels contents 
        checkIfHTMLValid listOfLabels

checkIfHTMLValid :: [String] -> IO()
checkIfHTMLValid xs = if checkValidHTMLTags xs then 
                (if checkTagsValid xs then
                (if checkTagsClosed xs && checkClosedTagPairs (reverse xs) then
                (if checkHeadAndTitle xs then
                (if checkHeadBody xs then
                (if checkNestedTags xs then
                (if checkPNesting xs then 
                putStr "HTML correct.\n" 
                else putStr "Error <p> <div> cannot be nested in <p>.\n")
                else putStr "Error with nesting of tags.\n")
                else putStr "Error with <head> <body> tags.\n")
                else putStr "Error with <head> <title> tags.\n")
                else putStr "Error unmatched closing tags.\n")
                else putStr "Error Invalid HTML tags.\n")
                else putStr "Error with <html> </html> tags.\n"


openTags = ["html", "head", "title", "body", "h1", "h2", "h3", "p", "ul", "li", "a", "div", "br", "hr"]
closeTags = ["/html", "/head", "/title", "/body", "/h1", "/h2", "/h3","/p", "/ul", "/li", "/a", "/div"]
        
tags :: [String] -> [String]
tags xs = [extractTag x | x <- xs]

labels :: String -> [String]
labels file = [x | x <- extractLabels file]    

extractLabels :: String -> [String]
extractLabels []       = []
extractLabels ('<':xs) = let (v,rest) = extractLabels' xs in v:extractLabels rest
extractLabels (_  :xs) = extractLabels xs

extractLabels' []       = ([], []) 
extractLabels' ('>':xs) = ([], xs)
extractLabels' (x  :xs) = let (v,rest) = extractLabels' xs in (x:v, rest)

extractTag :: String -> String
extractTag xs = case words xs of (x : _) -> x

checkTagsValid :: [String] -> Bool
checkTagsValid [] = True 
checkTagsValid (x:xs) | validTag x = checkTagsValid xs
                      | otherwise = False 

validTag :: String -> Bool
validTag x | isTagOpenTag x = checkTagPresent x openTags 
           | otherwise = checkTagPresent x closeTags

checkTagPresent :: String -> [String] -> Bool
checkTagPresent x [] = False
checkTagPresent x y = x `elem` y 

checkValidHTMLTags :: [String] -> Bool
checkValidHTMLTags [] = False    
checkValidHTMLTags xs = head xs == head openTags && last xs == head closeTags

checkHeadBody :: [String] -> Bool                            
checkHeadBody [] = False                            
checkHeadBody xs = if (fIndex (openTags!!1) xs < fIndex (closeTags!!1) xs 
                && fIndex (closeTags!!1) xs < fIndex (openTags!!3) xs
                && fIndex (openTags!!3) xs < fIndex (closeTags!!3) xs) 
                && (not (doesHeadExist $ drop (fIndex (closeTags!!3) xs) xs)) then True else False

checkHeadAndTitle :: [String] -> Bool
checkHeadAndTitle xs = if isTitlePresent xs then
                        if fIndex (openTags!!1) xs < fIndex (openTags!!2) xs
                        && fIndex (closeTags!!2) xs < fIndex (closeTags!!1) xs
                        && (not (isTitlePresent $ drop (fIndex (closeTags!!1) xs) xs))  then True else False 
                        else True 

doesHeadExist :: [String] -> Bool
doesHeadExist [] = False
doesHeadExist xs = (openTags!!1) `elem` xs || (closeTags!!1) `elem` xs 

isTitlePresent :: [String] -> Bool
isTitlePresent xs = (openTags!!2) `elem` xs && (closeTags!!2) `elem` xs

fIndex :: String -> [String] -> Int
fIndex x xs = getIndex $ elemIndex x xs

getIndex :: Maybe Int -> Int
getIndex (Just a) = a
getIndex Nothing  = -1

isTagOpenTag :: String -> Bool
isTagOpenTag x = if take 1 x /= "/" then True else False

notSingleTag :: String -> Bool
notSingleTag x = x /= openTags!!12 && x /= openTags!!13 

findCloseTag :: String -> String
findCloseTag x = closeTags!!(fIndex x openTags)

findOpenTag :: String -> String
findOpenTag x = openTags!!(fIndex x closeTags)

checkNestedTags :: [String] -> Bool
checkNestedTags [] = True
checkNestedTags (x:xs) | isTagOpenTag x && notSingleTag x = nestedTags x xs && checkNestedTags xs
                       | otherwise = checkNestedTags xs

nestedTags :: String -> [String] -> Bool
nestedTags y [] = False 
nestedTags y (x:xs) | isTagOpenTag x = True
                    | otherwise = if x == (findCloseTag y) then True else False

checkPNesting :: [String] -> Bool
checkPNesting [] = True
checkPNesting (x:xs) | x == (openTags!!7) = pNestedTags xs
                     | otherwise = checkPNesting xs

pNestedTags :: [String] -> Bool
pNestedTags [] = True
pNestedTags (x:xs) | x == (closeTags!!7) = checkPNesting xs
                   | x == (openTags!!7) || x == (openTags!!11) || x == (closeTags!!7) || x == (closeTags!!11) = False
                   | otherwise = pNestedTags xs          

checkTagsClosed :: [String] -> Bool
checkTagsClosed [] = True
checkTagsClosed (x:xs) | isTagOpenTag x && notSingleTag x = checkForClosingTag x xs && checkTagsClosed xs 
                       | otherwise = checkTagsClosed xs

                       
checkForClosingTag :: String -> [String] -> Bool
checkForClosingTag x [] = False
checkForClosingTag y xs | isTagOpenTag y = (findCloseTag y) `elem` xs
                            | otherwise = checkForClosingTag (head xs) xs

checkClosedTagPairs :: [String] -> Bool
checkClosedTagPairs [] = True
checkClosedTagPairs (x:xs) | isTagOpenTag x && notSingleTag x = checkClosedTagPairs xs
                           | otherwise = checkForTagPairs x xs && checkClosedTagPairs xs 

checkForTagPairs :: String -> [String] -> Bool
checkForTagPairs x [] = False
checkForTagPairs y xs | isTagOpenTag y = checkForTagPairs (head xs) xs 
                      | otherwise = (findOpenTag y) `elem` xs