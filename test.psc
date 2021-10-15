// This is a test file for pseudoc
entry soma {
    let a: int; 
    Out("Inserir A");
    In(a);
    let b: int;
    Out("Inserir B");
    In(b);
    let soma = a + b;
    Out("A soma de a + b é ", soma);

    if a == 1 {
        Out("A é igual a 1");
    } else if a == 2 {
        Out("A é igual a 2");
    } else if a == 3 {
        Out("A é igual a 3");
    } else {
        a = 1;
        Out("A é igual a 1 >:)");
    }
}
