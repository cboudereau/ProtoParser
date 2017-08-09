#I __SOURCE_DIRECTORY__
#r "packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "packages/FParsec/lib/net40-client/FParsec.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Drawing.dll"
#r "System.Numerics.dll"

open FParsec


type FieldSpec = 
    { Id : int
      Type: string
      Name : string }

type Field = 
    | Required of FieldSpec
    | Optional of FieldSpec
    | Repeated of FieldSpec

type Message = 
    { Name : string
      Fields : Field list } 

let pWord = (<>) ' ' |> satisfy |> manyChars

let (<!>) f x = x |>> f

let (<*>) f x = f >>= fun f' -> x >>= fun x' -> preturn (f' x')

let ws = pchar ' ' |> manyChars |>> ignore

let pField = 
    let fieldSpec f t n i = f { Id=i; Type=t; Name=n }
    let pRequired = stringCIReturn "required" (fieldSpec Required)
    let pOptional = stringCIReturn "optional" (fieldSpec Optional)
    let pRepeated = stringCIReturn "repeated" (fieldSpec Repeated)
    let pField = ws >>. (pRequired <|> pOptional <|> pRepeated)
    
    let pType = ws >>. pWord
    let pName = ws >>. (noneOf " =" |> manyChars)
    let pId = ws >>. pstring "=" >>. spaces >>. pint32

    pField <*> pType <*> pName <*> pId

let pFields = spaces >>. sepEndBy pField (spaces >>. pchar ';' .>> spaces)

run pFields """required string CalStarttime=1;optional string CalEndtime=2"""
run pFields """required string CalStarttime=1;optional string CalEndtime=2;"""
run pFields """required string CalStarttime=1; optional string CalEndtime=2;"""
run pFields """required string CalStarttime=1 ; optional string CalEndtime=2;"""
run pFields """
required string CalStarttime=1 ; optional string CalEndtime=2;"""
run pFields """
 required string CalStarttime=1 ; optional string CalEndtime=2;"""
run pFields """
    required string CalStarttime=1 ; optional string CalEndtime=2;"""
run pFields """
    required string CalStarttime=1;
    optional string CalEndtime=2;"""
run pFields """
    required string CalStarttime=1;
    optional string CalEndtime=2;
    
    """
run pFields """


    required string CalStarttime=1;
    optional string CalEndtime=2;
    
    """
run pFields """


    required string CalStarttime=1;

    optional string CalEndtime=2;
    
    """

let message name fields = { Fields=fields; Name=name }

let pMessage = spaces >>. pstringCI "message" >>. spaces >>. (noneOf "{ " |> manyChars) 

run pMessage """message CalculateInfo"""
run pMessage """message CalculateInfo """
run pMessage """ message CalculateInfo """
run pMessage """message 
CalculateInfo """
run pMessage """message CalculateInfo{"""
run pMessage """message CalculateInfo{"""

let pProto = pMessage |>> message .>> spaces .>> pchar '{' .>> spaces <*> pFields .>> spaces .>> pchar '}'

let pProtos = sepEndBy pProto spaces

let sample = """
    message CalculateInfo {
    required string CalStarttime=1;
    optional string CalEndtime=2;
    required string Smiles=3;
    optional string CAS=4;
    optional string ChName=5;
    optional string EnName=6;
    required string Param=7;
    required bytes Result=8;
    required bool IsFinished=9;
    required bool IsFinished=9; }

    message GetAllCalulateResponse{
        required bool  isSuccessful = 1;
        required int32 Count=2;
        repeated CalculateInfo History=3; }
"""


run pProtos sample