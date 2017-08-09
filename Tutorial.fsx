#I __SOURCE_DIRECTORY__
#r "packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "packages/FParsec/lib/net40-client/FParsec.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Drawing.dll"
#r "System.Numerics.dll"

open FParsec

let pWord = (<>) ' ' |> satisfy |> manyChars

let (<!>) f x = x |>> f

let (<*>) f x = f >>= fun f' -> x >>= fun x' -> preturn (f' x')

let ws = pchar ' ' |> manyChars |>> ignore

///https://developers.google.com/protocol-buffers/docs/proto3#scalar
module ScalarType = 
    let private mapping = 
        [ "double",  typeof<double>
          "float",   typeof<float>
          "int32",   typeof<int>
          "int64",   typeof<int64>
          "uint32",  typeof<uint32>
          "uint64",  typeof<uint64>
          "sint32",  typeof<int32>
          "sint64",  typeof<int64>
          "fixed32", typeof<uint32>
          "fixed64", typeof<uint64>
          "sfixed32",typeof<int32>
          "sfixed64",typeof<int64>
          "bool",    typeof<bool>
          "string",  typeof<string>
          "bytes",   typeof<byte[]> ]
        |> Map.ofList

    let parser = 
        mapping
        |> Map.toSeq
        |> Seq.map (fst >> pstring)
        |> Seq.fold (<|>) pzero

type Type = 
    | Scalar of string
    | Custom of string

let pType = 
    (ScalarType.parser |>> Scalar)
    <|> (ws >>. pWord |>> Custom)

type FieldSpec = 
    { Id : int
      Type: Type
      Name : string }

type Field = 
    | Required of FieldSpec
    | Optional of FieldSpec
    | Repeated of FieldSpec

type Message = 
    { Name : string
      Fields : Field list } 

let pField = 
    let fieldSpec f t n i = f { Id=i; Type=t; Name=n }
    let pRequired = stringCIReturn "required" (fieldSpec Required)
    let pOptional = stringCIReturn "optional" (fieldSpec Optional)
    let pRepeated = stringCIReturn "repeated" (fieldSpec Repeated)
    let pField = ws >>. (pRequired <|> pOptional <|> pRepeated)
    
    //TODO: fallback scalar to custom type
    let pName = ws >>. (noneOf " =" |> manyChars)
    let pId = ws >>. pstring "=" >>. spaces >>. pint32

    pField <*> (ws >>. pType) <*> (ws >>. pName) <*> pId

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

let pProtoMessage = pMessage |>> message .>> spaces .>> pchar '{' .>> spaces <*> pFields .>> spaces .>> pchar '}'

let pProtos = sepEndBy pProtoMessage spaces

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