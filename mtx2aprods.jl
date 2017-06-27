
function mtx2aprods(mtx :: String; compact = true)
    m = n = nz = s = st = 0
    open(mtx, "r") do file
        kline = 1
        lines = readlines(file)

        if split(lines[1])[1] != "%%MatrixMarket"
            error("Not a matrixmarket file!")
        end
        if split(lines[1])[2] != "matrix"
            error("Format not implemented!")
        end

        if split(lines[1])[3] != "coordinate"
            error("Format for matrix not implemented!")
        end

        qualifier = split(lines[1])[4]
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

        qualifier = split(lines[1])[5]
        if qualifier != "general" && qualifier != "symmetric" &&  qualifier != "skew-symmetric" && qualifier != "hermitian"
            error("Not an struct format valid!")
        end

        #comments of mtx files
        while split(lines[kline])[1][1] == '%'
            kline = kline+1
        end

        #dimensions of matrix
        spl = split(lines[kline])
        m, n, nz = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(Int64,spl[3])

        s = fill("0", m)
        st = fill("0", n)
        if compact
            if qualifier == "general"
                for kline = kline+1:length(lines)
                    spl = split(lines[kline])
                    if spl[1][1] == '%'
                        continue
                    end
                    i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                    sgn, aij = aij > 0 ? ("+", aij) : ("-", -aij)
                    s[i] = s[i] * "$sgn$aij*v[$j]"
                    st[j] = st[j] * "$sgn$aij*v[$i]"
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
                    if i != j
                        s[j] = s[j] * " $sgn $(aij')*v[$i]"
                        st[i] = st[i] * " $sgn $(aij')*v[$j]"
                    end
                end
            end
        end
    end

    open("Aprod.jl", "w") do file_Aprod
        write(file_Aprod,"function Aprod(v)")
        if compact
            write(file_Aprod," s=zeros($m);")
            for i = 1:m
                @printf(file_Aprod, "s[%d]=%s;", i, s[i])
            end
            write(file_Aprod,"return s; end\n")

            write(file_Aprod,"function Aprod(v)")
            write(file_Aprod," st=zeros($n);")
            for j = 1:n
                @printf(file_Aprod, "st[%d] = %s;", j, st[j])
            end
            write(file_Aprod,"return st; end")

        else
            write(file_Aprod,"\n    s = zeros($m)\n")
            for i = 1:m
                @printf(file_Aprod, "    s[%d] = %s\n", i, s[i])
            end
            write(file_Aprod,"    return s\nend\n")

            write(file_Aprod,"\nfunction Atprod(v)")
            write(file_Aprod,"\n    st = zeros($n)\n")
            for j = 1:n
                @printf(file_Aprod, "    st[%d] = %s\n", j, st[j])
            end
            write(file_Aprod,"    return st\nend")

        end
    end
    println("Sucess")
    return (m,n,nz)
end
