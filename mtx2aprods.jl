function mtx2aprods(mtx :: String; compact = true)
    m = n = nz = s = st = 0
    if compact
      cchar1 = cchar3 = ""
      cchar2 = ";"
    else
      cchar1 = " "
      cchar2 = "\n"
      cchar3 = "    "
    end
    open(mtx, "r") do file
        line = readline(file)
        if split(line)[1] != "%%MatrixMarket"
            error("Not a matrixmarket file!")
        end
        if split(line)[2] != "matrix"
            error("Format not implemented!")
        end
        if split(line)[3] != "coordinate"
            error("Format for matrix not implemented!")
        end
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
        if qualifier != "general" && qualifier != "symmetric" &&  qualifier != "skew-symmetric" && qualifier != "hermitian"
            error("Not an struct format valid!")
        end

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
                if spl[1][1] == '%'
                    continue
                end
                i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                sgn, aij = aij > 0 ? ("+", aij) : ("-", -aij)
                s[i] = s[i] * "$cchar1$sgn$cchar1$aij*v[$j]"
                st[j] = st[j] * "$cchar1$sgn$cchar1$aij*v[$i]"
                line = readline(file)
            end
        elseif qualifier == "symmetric"
              while line!=""
                spl = split(line)
                if spl[1][1] == '%'
                    continue
                end
                i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                sgn, aij = aij > 0 ? ("+", aij) : ("-", -aij)

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
                if spl[1][1] == '%'
                    continue
                end
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
                if spl[1][1] == '%'
                    continue
                end
                i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])+parse(entries,spl[4])*im
                sgn, aij = real(aij) > 0 ? ("+", aij) : ("-", -aij)
                s[i] = s[i] * "$cchar1$sgn$cchar1$aij*v[$j]"
                st[j] = st[j] * "$cchar1$sgn$cchar1$aij*v[$i]"
                if i != j
                    s[j] = s[j] * "$cchar1$sgn$cchar1$(aij')*v[$i]"
                    st[i] = st[i] * "$cchar1$sgn$cchar1$(aij')*v[$j]"
                end
                line = readline(file)
            end
        end
    end

    open("Aprod.jl", "w") do file_Aprod

        @printf(file_Aprod,"function Aprod(v) Av=zeros(%d)%s%sAv%s=%s[",n,cchar2,cchar3,cchar1,cchar1);
        for i = 1:m
            if s[i] != ""
                @printf(file_Aprod, "%s%s",  s[i],cchar2);
            end
        end
        @printf(file_Aprod,"]%s%sreturn Av%send\n\nfunction Atprod(v) Av=zeros(%d)%s%sAv%s=%s[",cchar2,cchar3,cchar2,m,cchar2,cchar3,cchar1,cchar1)
        for j = 1:n
            if st[j] !=""
                @printf(file_Aprod, "%s%s", st[j],cchar2)
            end
        end
        @printf(file_Aprod,"]%s%sreturn Av%send",cchar2,cchar3,cchar2)
    end
    println("Success")
    return (m,n,nz)
end
