Pro Split_pairs, snum, first = fir, second = sec

	on_error, 1

	Spec_file_check, snum, list = lis, nscan = nsc
	qy = (qz = fltarr(nsc))
	for i = 0l, nsc-1 do begin
		qy[i] = (Scan_column(lis[i],1))[0]
		qz[i] = (Scan_column(lis[i],2))[0]
	endfor
	qy = Fltround(qy,dig=3)
	qz = Fltround(qz,dig=3)
	if (max(qz,min=min) - min) gt 0 then message, 'Not a constant Qz!'
	exs = 0
	j = 0l
	repeat begin 
		if exs then fir = [fir,lis[j]] else fir = lis[j]
		if qy[j+1] eq qy[j] then j = j+1 $
		else message, 'Missing scan at ' + string(lis[j],form='(i0)'),/con
		if exs then sec = [sec,lis[j]] else sec = lis[j]
		exs = 1
		j = j+1
	endrep until j ge nsc

	return
end
	