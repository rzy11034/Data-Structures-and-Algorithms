unit DSA.Interfaces.Comparer;

interface

uses
  System.Generics.Defaults;

type

  IComparer<T> = interface
    ['{E7B0E421-33B7-4BF7-8490-7DBFA040BE94}']
    function Compare(const Left, Right: T): integer;
  end;

  TComparer<T> = class(TInterfacedObject, IComparer<T>)
  private
    __c: System.Generics.Defaults.IComparer<T>;
  public
    function Compare(const Left, Right: T): integer;
    constructor Default;
  end;

implementation

{ TComparer<T> }

constructor TComparer<T>.Default;
begin
  __c := System.Generics.Defaults.TComparer<T>.Default;
end;

function TComparer<T>.Compare(const Left, Right: T): integer;
begin
  Result := __c.Compare(Left, Right);
end;

end.
