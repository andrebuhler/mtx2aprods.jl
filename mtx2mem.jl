function mtx2mem(mtx :: String; compact = true)
    m = n = nz = 0
    compact ? A=spzeros(1,1) : A=zeros(1,1)
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
        compact ? A=spzeros(m,n) : A=zeros(m,n)

        if qualifier == "general"
          while line!=""
                spl = split(line)
                i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                A[i,j]=aij
                line = readline(file)
            end
        elseif qualifier == "symmetric"
              while line!=""
                spl = split(line)
                i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                A[i,j]=aij
                if i != j
                    A[j,i]=aij
                end
                line = readline(file)
            end
        elseif qualifier == "skew-symmetric"
            while line!=""
                spl = split(line)
                i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])
                A[i,j]=aij
                if i != j
                    A[j,i]=-aij
                end
                line = readline(file)
            end
        elseif qualifier == "hermitian"
              while line!=""
                spl = split(line)
                i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(entries,spl[3])+parse(entries,spl[4])*im
                A[i,j]=aij
                if i != j
                    A[j,i]=aij*-im
                end
                line = readline(file)
            end
        end
    end

    open("A.jl", "w") do file_Aprod
        @printf(file_Aprod, "A=[");
        for i = 1:m
            for j=1:n
                @printf(file_Aprod, "%f ",  A[i,j]);
            end
            @printf(file_Aprod,";")
        end
        @printf(file_Aprod,"]")

    end
end
