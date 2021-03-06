module Save exposing (  SaveModel 
                    --   ,Comp03_SaveModel 
                    --   ,Comp04_SaveModel 
                    --   ,Comp05_SaveModel 
                       )

{--
import Page.Component03.Home  exposing (SaveModel)
import Page.Component04.Home  exposing (SaveModel)
import Page.Component05.Home  exposing (SaveModel)

type  alias SaveModel
      =  {
            comp01: {save : Bool, model : {} }
           ,comp02: {save : Bool, model : {} }
           -- ,comp03: {save : Bool, model : {counter : Int} }
           ,comp03: {save : Bool, model : Comp03.SaveModel }
           -- ,comp04: {save : Bool, model : {counter : Int} }
           ,comp04: {save : Bool, model : Comp04.SaveModel }
           ,comp05: {save : Bool, model : Comp05.SaveModel }
         }
--}

import Color exposing (Color) 
--ADD COMPONENT-- SaveModel

type  alias SaveModel
      =  {
            comp01: {save : Bool, model : {} }
           ,comp02: {save : Bool, model : {} }
           -- ,comp03: {save : Bool, model : {counter : Int} }
           ,comp03: {save : Bool, model : Comp03_SaveModel }
           -- ,comp04: {save : Bool, model : {counter : Int} }
           ,comp04: {save : Bool, model : Comp04_SaveModel }
           ,comp05: {save : Bool, model : Comp05_SaveModel }
         }

type alias Comp03_SaveModel
     =  {
          counter : Int
        }
type alias Comp04_SaveModel
     =  {
          counter : Int
        , color1  : Color.Color
        , color2  : Color.Color
        }
type alias Comp05_SaveModel
     =  {
          counter : Int
        }

