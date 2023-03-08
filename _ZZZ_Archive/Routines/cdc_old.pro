Pro CDC_old, key, fnam, code = cdc, decode = dcd, _extra = _e

	on_error, 1
	wext = ['.txc','.txt']
	dext = strmid(wext,1)

	klen = strlen(Default(key,'',/dtyp))
	if klen ge 4 or klen le 96 then bkey = byte(key) $
	else message, 'Code string should have at least 4 characters!'

	ifnam = File_get(fnam,stat=istat,/read,_extra=_e)
	if istat then begin
		nam = Fnamparse(ifnam,pat=pat,ext=ext)
		dfl = One_of(cdc,dcd)
		if dfl eq -1 then dfl = Streq(ext,wext[0])
		ofnam = pat + nam + wext[dfl]

		if dfl then begin
			openr, iun, ifnam, /get_lun
			len = (fstat(iun)).size
			blin = bytarr(len)
			readu, iun, blin
			free_lun, iun
			check = blin[0:1]
			blin = blin[2:*]
			len = len-2
			bcheck = (byte(check) + 128b - bkey[-2:-1]) mod 128b
			shf = long(string(bcheck))
			skey = string(shift(bkey,shf))
			bkey = (byte(replicate(skey,len/klen+1)))[0:len-1]
			blin = (blin + 128b - bkey) mod 128b
			dum = Strparse_mm(string(blin),string(127b),lin)
			dum = where(lin eq ' ',ndum)
			if ndum gt[0] then lin[dum] = ''
			ofnam = $
			File_get(ofnam,def=dext[dfl],/write,/over,stat=ostat,_extra=_e)
			if ostat then begin
				openw, oun, ofnam, /get_lun
				printf, oun, lin, form = '(a)'
				free_lun, oun
			endif else message, 'Error opening output file!'
		endif else begin
			dum = Rascline(ifnam,lines=lin,count=con)
			if con gt 0 then begin
				dum = where(lin eq '',ndum)
				if ndum gt 0 then lin[dum] = ' '
				blin = byte(strjoin(lin + string(127b)))
				shf = round(0.5 + (klen-1)*randomu(s))
				pref = (byte(string(shf,form='(i02)')) + bkey[-2:-1]) mod 128b
				skey = string(shift(bkey,shf))
				len = n_elements(blin)
				bkey = (byte(replicate(skey,len/klen+1)))[0:len-1]
				blin = (blin + bkey) mod 128b
				blin = [pref,blin]
				ofnam = $
				File_get(ofnam,def=dext[dfl],/write,/over,stat=ostat,_extra=_e)
				if ostat then begin
					openw, oun, ofnam, /get_lun
					writeu, oun, blin
					free_lun, oun
				endif else message, 'Error opening output file!'
			endif else message, 'Empty file, nothing to code!'
		endelse
	endif else message, 'Error opening input file!'

	return
end