module Server.Password
    ( hash
    , equalsHash
    , isValidPassword
    ) where

import Data.Password.Bcrypt
import Data.Text (pack, unpack, Text)
import qualified Data.Password.Validate as DPV (isValidPassword)
import Data.Password.Validate (defaultPasswordPolicy_)

hash :: Text -> IO (PasswordHash Bcrypt)
hash password =
    hashPassword $ mkPassword password

equalsHash :: Text -> PasswordHash Bcrypt -> Bool
equalsHash password hash =
    PasswordCheckSuccess == checkPassword
        (mkPassword password) hash

isValidPassword :: Text -> Bool
isValidPassword password = DPV.isValidPassword defaultPasswordPolicy_ $ mkPassword password
