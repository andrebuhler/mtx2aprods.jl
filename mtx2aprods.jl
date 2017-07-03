function mtx2aprods(mtx :: String; compact = true)
    m = n = nz = s = st = 0
    compact ? (cchar1 = cchar3 = ""; cchar2 = ";") : (cchar1 = " ";cchar2 = "\n"; cchar3 = "    ")
    open(mtx, "r") do file
        line = readline(file)
        split(line)[1] != "%%MatrixMarket" ? error("Not a matrixmarket file!") :
        split(line)[2] != "matrix" ? error("Format not implemented!") :
        split(line)[3] != "coordinate" ? error("Format for matrix not implemented!") :
        qualifier = split(line)[4]

        if qualifier == "real"
            entries = Float64
        elseif qualifier == "integer"
            entries = Int64
        elseif qualifier == "complex"
            entries = Float64
        elseif qualifier == "pattern"
            error("Entries format not implemented!")
        else
            error("Not an entries format valid!")
        end

        qualifier = split(line)[5]
        (qualifier != "general" && qualifier != "symmetric" &&  qualifier != "skew-symmetric" && qualifier != "hermitian") ? error("Not an struct format valid!") :


        #comments of mtx files
        while split(line)[1][1] == '%'
            line = readline(file)
        end

        #dimensions of matrix
        spl = split(line)
        m, n, nz = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(Int64,spl[3])
        line = readline(file)
        s = fill("", m)
        st = fill("", n)

        if qualifier == "general"
          while line!=""
                spl = split(line)
                i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                sgn = aij > 0 ? "+" : ""
                s[i] = s[i] * "$cchar1$sgn$cchar1$aij*v[$j]"
                st[j] = st[j] * "$cchar1$sgn$cchar1$aij*v[$i]"
                line = readline(file)
            end
        elseif qualifier == "symmetric"
              while line!=""
                spl = split(line)
                i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                sgn = aij > 0 ? "+" : ""

                s[i] = s[i] * "$cchar1$sgn$cchar1$aij*v[$j]"
                st[j] = st[j] * "$cchar1$sgn$cchar1$aij*v[$i]"

                if i != j
                    s[j] = s[j] * "$cchar1$sgn$cchar1$aij*v[$i]"
                    st[i] = st[i] * "$cchar1$sgn$cchar1$aij*v[$j]"
                end
                line = readline(file)
            end
        elseif qualifier == "skew-symmetric"
            while line!=""
                spl = split(line)
                i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                sgn, isgn, aij = aij > 0 ? ("+", "-", aij) : ("-", "+",-aij)
                s[i] = s[i] * "$cchar1$sgn$cchar1$aij*v[$j]"
                st[j] = st[j] * "$cchar1$sgn$cchar1$aij*v[$i]"
                if i != j
                    s[j] = s[j] * "$cchar1$isgn$cchar1$aij*v[$i]"
                    st[i] = st[i] * "$cchar1$isgn$cchar1$aij*v[$j]"
                end
                line = readline(file)
            end
        elseif qualifier == "hermitian"
              while line!=""
                spl = split(line)
                i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])+parse(entries,spl[4])*im
                sgn, aij = real(aij) > 0 ? ("+", aij) : ("-", -aij')
                s[i] = s[i] * "+($cchar1$sgn$cchar1$aij)*v[$j]"
                st[j] = st[j] * "+($cchar1$sgn$cchar1$aij)*v[$i]"
                if i != j
                    s[j] = s[j] * "+($cchar1$sgn$cchar1$(aij'))*v[$i]"
                    st[i] = st[i] * "+($cchar1$sgn$cchar1$(aij'))*v[$j]"
                end
                line = readline(file)
            end
        end
    end

    open("Aprod.jl", "w") do file_Aprod
        @printf(file_Aprod,"function Aprod(Av,v) %s%sAv%s=%s[",cchar2,cchar3,cchar1,cchar1);
        for i = 1:m
            @printf(file_Aprod, "%s%s",  s[i],cchar2);
        end
        @printf(file_Aprod,"]%s%sreturn Av%send\n\nfunction Atprod(Av,v) %s%sAv%s=%s[",cchar2,cchar3,cchar2,cchar2,cchar3,cchar1,cchar1)
        for j = 1:n
            @printf(file_Aprod, "%s%s", st[j],cchar2)
        end
        @printf(file_Aprod,"]%s%sreturn Av%send",cchar2,cchar3,cchar2)
    end
end
