
function mtx2aprods(mtx :: String; compact = true)
    m = n = nz = s = st = 0
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

        s = fill("", m)
        st = fill("", n)
        if compact
            if qualifier == "general"
              while line!=""
                    spl = split(line)
                    if spl[1][1] == '%'
                        continue
                    end
                    i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                    sgn, aij = aij > 0 ? ("+", aij) : ("-", -aij)
                    s[i] = s[i] * "$sgn$aij*v[$j]"
                    st[j] = st[j] * "$sgn$aij*v[$i]"
                    line = readline(file)
                end
            elseif qualifier == "symmetric"
                for kline = kline+1:length(lines)
                    spl = split(lines[kline])
                    if spl[1][1] == '%'
                        continue
                    end
                    i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                    sgn, aij = aij > 0 ? ("+", aij) : ("-", -aij)

                    s[i] = s[i] * "$sgn$aij*v[$j]"
                    st[j] = st[j] * "$sgn$aij*v[$i]"

                    if i != j
                        s[j] = s[j] * "$sgn$aij*v[$i]"
                        st[i] = st[i] * "$sgn$aij*v[$j]"
                    end
                end
            elseif qualifier == "skew-symmetric"
                for kline = kline+1:length(lines)
                    spl = split(lines[kline])
                    if spl[1][1] == '%'
                        continue
                    end
                    i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                    sgn, isgn, aij = aij > 0 ? ("+", "-", aij) : ("-", "+",-aij)
                    s[i] = s[i] * "$sgn$aij*v[$j]"
                    st[j] = st[j] * "$sgn$aij*v[$i]"
                    if i != j
                        s[j] = s[j] * "$isgn$aij*v[$i]"
                        st[i] = st[i] * "$isgn$aij*v[$j]"
                    end
                end
            elseif qualifier == "hermitian"
                for kline = kline+1:length(lines)
                    spl = split(lines[kline])
                    if spl[1][1] == '%'
                        continue
                    end
                    i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])+parse(entries,spl[4])*im
                    sgn, aij = real(aij) > 0 ? ("+", aij) : ("-", -aij)
                    s[i] = s[i] * "$sgn$aij*v[$j]"
                    st[j] = st[j] * "$sgn$aij*v[$i]"
                    if i != j
                        s[j] = s[j] * "$sgn$(aij')*v[$i]"
                        st[i] = st[i] * "$sgn$(aij')*v[$j]"
                    end
                end
            end
        else
            if qualifier == "general"
                for kline = kline+1:length(lines)
                    spl = split(lines[kline])
                    if spl[1][1] == '%'
                        continue
                    end
                    i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                    sgn, aij = aij > 0 ? ("+", aij) : ("-", -aij)
                    s[i] = s[i] * " $sgn $aij*v[$j]"
                    st[j] = st[j] * " $sgn $aij*v[$i]"

                end
            elseif qualifier == "symmetric"
                for kline = kline+1:length(lines)
                    spl = split(lines[kline])
                    if spl[1][1] == '%'
                        continue
                    end
                    i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                    sgn, aij = aij > 0 ? ("+", aij) : ("-", -aij)
                    s[i] = s[i] * " $sgn $aij*v[$j]"
                    st[j] = st[j] * " $sgn $aij*v[$i]"
                    if i != j
                        s[j] = s[j] * " $sgn $aij*v[$i]"
                        st[i] = st[i] * " $sgn $aij*v[$j]"
                    end
                end
            elseif qualifier == "skew-symmetric"
                for kline = kline+1:length(lines)
                    spl = split(lines[kline])
                    if spl[1][1] == '%'
                        continue
                    end
                    i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                    sgn, isgn, aij = aij > 0 ? ("+", "-", aij) : ("-", "+",-aij)
                    s[i] = s[i] * " $sgn $aij*v[$j]"
                    st[j] = st[j] * " $sgn $aij*v[$i]"
                    if i != j
                        s[j] = s[j] * " $isgn $aij*v[$i]"
                        st[i] = st[i] * " $isgn $aij*v[$j]"
                    end
                end
            elseif qualifier == "hermitian"
                for kline = kline+1:length(lines)
                    spl = split(lines[kline])
                    if spl[1][1] == '%'
                        continue
                    end
                    i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])+parse(entries,spl[4])*im
                    sgn, aij = real(aij) > 0 ? ("+", aij) : ("-", -aij)
                    s[i] = s[i] * " $sgn $aij*v[$j]"
                    st[j] = st[j] * " $sgn $aij*v[$i]"
                    s[i] = s[i] * " $sgn $aij*v[$j]"
                    st[j] = st[j] * " $sgn $aij*v[$i]"
                    if i != j
                        s[j] = s[j] * " $sgn $(aij')*v[$i]"
                        st[i] = st[i] * " $sgn $(aij')*v[$j]"
                    end
                end
            end
        end
    end

    open("Aprod.jl", "w") do file_Aprod
        if compact
            write(file_Aprod,"function Aprod(v,s) ")
            for i = 1:m
                @printf(file_Aprod, "s[%d]=%s;", i, s[i])
            end
            write(file_Aprod,"return s; end\n")

            write(file_Aprod,"\nfunction Atprod(v,st) ")
            for j = 1:n
                @printf(file_Aprod, "st[%d] = %s;", j, st[j])
            end
            write(file_Aprod,"return st; end")

        else
            write(file_Aprod,"function Aprod(v,s)\n")
            for i = 1:m
                if s[i] != ""
                  @printf(file_Aprod, "    s[%d] = %s\n", i, s[i])
                end
            end
            write(file_Aprod,"    return s\nend\n")

            write(file_Aprod,"\nfunction Atprod(v,st)\n")
            for j = 1:n
                @printf(file_Aprod, "    st[%d] = %s\n", j, st[j])
            end
            write(file_Aprod,"    return st\nend")

        end
    end
    println("Sucess")
    return (m,n,nz)
end
