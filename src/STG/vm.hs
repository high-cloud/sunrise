




data State =
  Code
  ArgumentStack -- ^ contains values
  ReturnStack
  UpdataStack -- ^ update frames
  Heap
  GlobalEnvironment

data Value
  = Addr -- ^ address in head
  | Int -- ^ primitive interger

