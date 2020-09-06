module Save exposing (  SaveModel )

--ADD COMPONENT-- SaveModel
type  alias SaveModel
      =  {
            comp01: {save : Bool, model : {} }
           ,comp02: {save : Bool, model : {} }
           ,comp03: {save : Bool, model : {counter : Int} }
           ,comp04: {save : Bool, model : {counter : Int} }
         }
