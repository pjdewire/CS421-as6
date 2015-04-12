
fun compGen(name,args) = (app Main.compile args; 0);
SMLofNJ.exportFn("toy", compGen);
