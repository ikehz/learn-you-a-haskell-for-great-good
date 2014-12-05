import Data.List (break)

x -: f = f x

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem  
myDisk = 
    Folder "root"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (i, (FSCrumb name xs ys):cs) = (Folder name (xs++[i]++ys), cs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder fname items, cs) = (namedFSItem, (FSCrumb fname prefix suffix):cs)
    where (prefix, namedFSItem:suffix) = break (nameIs name) items

nameIs :: Name -> FSItem -> Bool
nameIs name (File fname _) = name == fname
nameIs name (Folder fname _) = name == fname

fsRename :: Name -> FSZipper -> FSZipper
fsRename name (File _ d, cs) = (File name d, cs)
fsRename name (Folder _ is, cs) = (Folder name is, cs)

fsNew :: FSItem -> FSZipper -> FSZipper
fsNew item (Folder name is, cs) = (Folder name (item:is), cs)
