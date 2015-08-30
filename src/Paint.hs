module Paint
    ( module Paint.DisplayCommand
    , module Paint.Rect
    , module Paint.Text
    , paint
    ) where

import Paint.DisplayCommand
import Paint.Rect
import Paint.Text

paint :: SDLContext -> [DisplayCommand] -> IO ()
paint ctx = mapM_ ($ ctx)
