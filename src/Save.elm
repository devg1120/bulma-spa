module Save exposing (  SaveModel 
                       ,Comp03_Model 
                       ,Comp04_Model 
                       ,Comp05_Model 
                       )


--ADD COMPONENT-- SaveModel
type  alias SaveModel
      =  {
            comp01: {save : Bool, model : {} }
           ,comp02: {save : Bool, model : {} }
           -- ,comp03: {save : Bool, model : {counter : Int} }
           ,comp03: {save : Bool, model : Comp03_Model }
           -- ,comp04: {save : Bool, model : {counter : Int} }
           ,comp04: {save : Bool, model : Comp04_Model }
           ,comp05: {save : Bool, model : Comp05_Model }
         }

type alias Comp03_Model
     =  {
          counter : Int
        }
type alias Comp04_Model
     =  {
          counter : Int
        }
type alias Comp05_Model
     =  {
          counter : Int
        }
