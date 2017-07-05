
function mtx2jl(mtx)
	file = open(mtx)
	kline = 1
	nz = i = j = 0
	lines = readlines(file)
	#comments of mtx files
	while lines[kline][1] == lines[1][1]
		kline = kline+1
	end

	#dimensions of matrix
	spl = split(lines[kline])
	i,j,nz = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(Int64,spl[3])
	kline = kline+1
	A=zeros(i,j)

	while kline<=size(lines,1)
	   	spl = split(lines[kline])
			i,j,aij = parse(Int64,spl[1]),parse(Int64,spl[2]),parse(Float64,spl[3])
			A[i,j]=aij

			kline = kline+1
	end

	close(file)
	return A;
end
