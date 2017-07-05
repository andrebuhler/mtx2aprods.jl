# O arquivo "errado.mtx" é executado para se descartar o primeiro resultado, uma
# vez que é alocado coisas na primeira iteração do laço for.

lista = ["errado.mtx";"LFAT5.mtx";"Stranke94.mtx";"Journals.mtx";"Tref200.mtx";"b1_ss.mtx";"rand1.mtx";"rand2.mtx";"LFAT5anti.mtx";"anti1.mtx";"Trefanti.mtx";"herm1.mtx";"herm2.mtx";]
cont = 0;
n = length(lista);
result1 = zeros(Int64,n,3);
result2 = zeros(n,2);
result3 = zeros(n,2);

for arq in lista
    println(arq)
    cont = cont+1;

    tmp = @timed (include("mtx2aprods.jl");(m,n,nz) = mtx2aprods(arq;compact = true))
    result1[cont,1] = m;
    result1[cont,2] = n;
    result1[cont,3] = nz;
    v = rand(n); Av = zeros(m);
    result2[cont,1] = round(tmp[3]/(1024^2),4);
    include("Aprod.jl");
    tmp = @timed (Aprod(Av,v);)
    result3[cont,1] = round(tmp[3]/(1024^2),4);

    tmp = @timed (include("mtx2mem.jl");(m,n,nz,A) = mtx2mem(arq;compact = true);)
    result2[cont,2] = round(tmp[3]/(1024^2),4);
    tmp = @timed (A*v;)
    result3[cont,2] = round(tmp[3]/(1024^2),4);
end
[lista result1 result2 result3]
