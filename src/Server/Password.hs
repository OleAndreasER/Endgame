module Server.Password
    ( hash
    , equalsHash
    ) where

import Data.Password.Bcrypt
import Data.Text (pack, unpack, Text)

hash :: Text -> IO (PasswordHash Bcrypt)
hash password =
    hashPassword $ mkPassword password

equalsHash :: Text -> PasswordHash Bcrypt -> Bool
equalsHash password hash =
    PasswordCheckSuccess == checkPassword
        (mkPassword password) hash
