module CLI.Endgame.Help
    ( displayHelp
    , displayLiftsHelp
    , displayProgramHelp
    ) where

displayHelp :: IO ()
displayHelp = putStrLn
    "Get started by creating a profile:\n\
    \  endgame profile new\n\n\
    \Switch profile:\n\
    \  endgame profile {name}\n\n\
    \View your first workout:\n\
    \  endgame next\n\
    \  endgame next {amount}\n\n\
    \Add it to your logs:\n\
    \  endgame add\n\n\
    \View your latest logs:\n\
    \  endgame logs\n\
    \  endgame logs {amount}\n\n\
    \View or edit a specific log:\n\
    \  endgame log\n\
    \  endgame log {n}\n\
    \  endgame log {n} fail {lift}\n\n\
    \View or set your bodyweight:\n\
    \  endgame bw\n\
    \  endgame bw {new bodyweight}\n\n\
    \View your lifts' stats:\n\
    \  endgame lifts\n\n\
    \Commands for editing your stats:\n\
    \  endgame lifts help\n\n\
    \View your program:\n\
    \  endgame program\n\n\
    \Commands for editing your program:\n\
    \  endgame program help\n"

displayLiftsHelp :: IO ()
displayLiftsHelp = putStrLn
    "Set pr for a lift:\n\
    \  endgame lifts pr {lift} {weight}\n\n\
    \Set progression increment:\n\
    \  endgame lifts progression {lift} {increment}\n\n\
    \Set cycle of lift:\n\
    \  endgame lifts cycle {lift} {position} {length}\n\n\
    \Toggle if a lift should be a bodyweight movement:\n\
    \  endgame lifts toggle-bodyweight {lift}\n"

displayProgramHelp :: IO ()
displayProgramHelp = putStrLn
    "View or edit a lift group cycle:\n\
    \  endgame program lift-group-cycle {n}\n\
    \  endgame program lift-group-cycle {n} edit\n\n\
    \View or edit a lift:\n\
    \  endgame program lift {lift}\n\
    \  endgame program lift {lift} edit\n"
