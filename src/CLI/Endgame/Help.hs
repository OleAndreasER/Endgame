module CLI.Endgame.Help
    ( displayHelp
    ) where

displayHelp :: IO ()
displayHelp = putStrLn
    "Get started by creating a profile:\n\
    \  endgame new profile\n\n\
    \Switch profile:\n\
    \  endgame profile {name}\n\n\
    \View your first workout:\n\
    \  endgame next\n\
    \  endgame next {n}\n\n\
    \Add it to your logs:\n\
    \  endgame add\n\n\
    \View your logs:\n\
    \  endgame logs\n\
    \  endgame logs {n}\n\n\
    \View a specific log:\n\
    \  endgame log\n\
    \  endgame log {n}\n\n\
    \Remove log:\n\
    \  endgame remove log\n\
    \  endgame remove log {n}\n\n\
    \View your lifts:\n\
    \  endgame lifts\n\n\
    \Set the PR or cycle of a lift:\n\
    \  endgame pr {lift} {new PR}\n\
    \  endgame cycle {lift} {position} {length}\n\n\
    \View or set your bodyweight:\n\
    \  endgame bodyweight\n\
    \  endgame bodyweight {new bodyweight}\n\n\
    \View your program:\n\
    \  endgame program\n\n\
    \Toggle whether a lift is considered a bodyweight movement:\n\
    \  endgame toggle bodyweight {lift}\n"
