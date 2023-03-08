Function Read_MCA, filename, norm = nrm, status = sta, _extra = _e

	on_error, 1

	res = -1l
	dum = Rascline(filename,lines=lin,count=con,stat=sta,_extra=_e)
	if sta then begin
		lin = strtrim(lin,1)
		fir = (where(Streq(lin,'@A',2),nfir))[0]
		if nfir gt 0 then begin
			dum = (where(Streq(lin,'#@CALIB',7),ndum))[0]
			if ndum eq 1 then begin
				ddum = Strparse_mm(lin[dum],'	 ',lis)
				ccoef = 0.001*lis[1:2]
			endif else ccoef = [0.,1.]
			if keyword_set(nrm) then begin
				nrmfl = 1
				dum = (where(Streq(lin,'#Monc',5),ndum))[0]
				if ndum eq 1 then begin
					ddum = Strparse_mm(lin[dum],'	 ',lis)
					ncoef = 1./lis[1]
				endif else ncoef = 1.
			endif else nrmfl = 0
			dlin = lin[fir:*]
			dlin[0] = strmid(dlin[0],2)
			glob = strjoin(dlin,' ')
			npo = Strparse_mm(glob,'	 \',sres)
			res = fltarr(3,npo+1)
			res[0,*] = Poleval(findgen(npo+1),ccoef)
			res[1,*] = float(sres)
			res[2,*] = sqrt(res[1,*])
			if nrmfl then res = Scan_scale(res,ncoef)
		endif else begin
			message, 'No MCA data found!', /con
			sta = 0
		endelse
	endif else message, 'file not found!', con

	return, res
end
		