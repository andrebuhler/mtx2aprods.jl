
function mtx2aprods(mtx; compact = true)
  m = n = s = st = 0
  open(mtx, "r") do file
    kline = 1
    lines = readlines(file)

    #comments of mtx files
    while split(lines[kline])[1][1] == '%'
      kline = kline+1
    end

    #dimensions of matrix

    spl = split(lines[kline])
    m, n, nz = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(Int64,spl[3])

      s = fill("0", m)
      st = fill("0", n)

    for kline = kline+1:length(lines)
      spl = split(lines[kline])
      if spl[1][1] == '%'
        continue
      end
      i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(Float64,spl[3])
      sgn, aij = aij > 0 ? ("+", aij) : ("-", -aij)

      if compact
        s[i] = s[i] * "$sgn$aij*v[$j]"
        st[j] = st[j] * "$sgn$aij*v[$i]"
      else
        s[i] = s[i] * "$sgn $aij*v[$j]"
        st[j] = st[j] * "$sgn $aij*v[$i]"
      end
    end
  end

  open("Aprod.jl", "w") do file_Aprod

    write(file_Aprod,"function Aprod(v)")
    if compact
      write(file_Aprod," s=[")
      for i = 1:m
        @printf(file_Aprod, "s[%d]=%s;", i, s[i])
      end
      write(file_Aprod,"];return s; end\n")
    else
      write(file_Aprod,"\ns = [\n")
      for i = 1:m
        @printf(file_Aprod, "s[%d] = %s\n", i, s[i])
      end
      write(file_Aprod,"]\nreturn s\n end\n")
    end

  end

  open("Aprod.jl", "a") do file_Atprod

    write(file_Atprod,"function Atprod(v)")
    if compact
      write(file_Atprod," st=[")
      for j = 1:n
        @printf(file_Atprod, "st[%d] = %s;", j, st[j])
      end
      write(file_Atprod,"];return st; end")
    else
      write(file_Atprod,"\nst = [\n")
      for j = 1:n
        @printf(file_Atprod, "st[%d] = %s\n", j, st[j])
      end
      write(file_Atprod,"]\nreturn st\n end")
    end
  end


  return
end
