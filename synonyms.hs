type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook
phoneBook = [("Rodrigo", "3226-9772")
            ,("Sybila", "9232-7098")
            ,("Carlos", "3765-0987")
            ]

getPhone :: PhoneBook -> Name -> Maybe PhoneNumber
getPhone [] _ = Nothing
getPhone (register:xs) name
  | name == fst register = Just (snd register)
  | otherwise = getPhone xs name

