module Server.Password
    ( hash
    , equalsHash
    ) where

import Data.Password.Bcrypt
import Data.Text (pack, unpack)

hash :: String -> IO (PasswordHash Bcrypt)
hash password =
    hashPassword $ mkPassword $ pack password

equalsHash :: String -> PasswordHash Bcrypt -> Bool
equalsHash password hash =
    PasswordCheckSuccess == checkPassword
        (mkPassword $ pack password) hash
