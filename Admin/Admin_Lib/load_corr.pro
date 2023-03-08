Pro Load_corr, expid = xid, station = sta

	on_error, 1

	ds = sdep(/ds)
	source = getenv('root_mm') + 'Admin' + ds + 'Data' + ds + 'corr_list.dat'
	dum = Rascline(source,lines=lin,count=n)

	if n gt 0 then begin
		xid = lonarr(n)
		sta = strarr(n)
		j = 0l
		read = 0
		for i = 0l, n-1 do begin
			if read then begin
				tem = Strparse_mm(lin[i],'	 ',lis)
				case tem of
					-1	:
					0	:	if Streq(lis[0],'end') then read = 0 $
							else message, 'Correction Data problem!'
					1	:	begin
								xid[j] = lis[0]
								sta[j] = lis[1]
								j = j + 1
							end
					else:	message, 'Correction Data problem!'
				endcase
			endif else if Streq(strtrim(lin[i],2),'begin') then read = 1
		endfor
		if j gt 0 then begin
			xid = xid[0:j-1]
			sta = sta[0:j-1]
		endif else xid = (sta = [])
	endif else message, 'No Data!

	return
end