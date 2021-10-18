// This is a test file for pseudoc
program soma;

body {
    let a: int; 
    Out("Inserir A");
    In(a);
    let b: int;
    Out("Inserir B");
    In(b);
    let total = a + b;
    Out("A soma de a + b é ", total);

    if a == 1 {
        Out("A é igual a 1");
    } else if a == 2 {
        if true {
            Out("Obamaprisma");
        }
        Out("A é igual a 2");
    } else if a == 3 {
        Out("A é igual a 3");
    } else {
        a = 1;
        Out("A é igual a 1 >:)");
    }
    Out("Podes ignorar isto");
}
