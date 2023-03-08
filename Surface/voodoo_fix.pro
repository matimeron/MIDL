Pro Voodoo_fix, filename = fnam, _extra = _e

	on_error, 1

	ptarr = Rascline(fnam,lin=farr,count=nl,file_name=firnam,call=2)
	sarr = strarr(nl)
	farr = strtrim(farr,1)
	slin = [where(Streq(farr,'#S',2),nslin),nl]
	jend = slin[1:*] - slin - 1
	sarr[0:slin[0]-1] = farr[0:slin[0]-1]

	for i = 0l, nslin-1 do begin
		ftem = farr[slin[i]:slin[i+1]-1]
		inds = lonarr(jend[i]+1)
		jl = (where(Streq(ftem,'#L',2),njl))[0]
		if njl eq 1 and jl lt jend[i] then begin
			jj = where(Streq(ftem,'#J',2),njj)
			if njj ge 1 and jj[0] gt jl and jj[(njj-1)>0] lt jend[i] then begin
				check = strtrim(ftem[jj[0]+1],2)
				if strlen(check) gt 0 then begin
					if Streq(strmid(check,0,1),'#') then jjoff = jj + 2 $
					else jjoff = jj + 1
				endif else jjoff = jj + 2
				dum = Strparse_mm(ftem[jj[0]],' 	',lis)
				nchan = lis[2]
				check = ftem[jjoff[0]]
				nro = nchan/(Strparse_mm(check,' 	') + 1)
				check = ftem[jjoff[0] + nro:jjoff[0] + nro + 2]
				dum = (where(check ne '',ndum))[0]
				lloff = jjoff + nro + dum
				ltem = ftem[lloff]
				inds[lloff] = -1
				if not Streq(strmid(ltem[njj-1],0,1),'#') then begin
					check = ftem[lloff[njj-1]+2]
					if Streq(strmid(check,0,1),'P') then begin
						ltem = [ltem,ftem[lloff[njj-1]+1:lloff[njj-1]+4]]
						inds[lloff[njj-1]+1:lloff[njj-1]+4] = -1
					endif
				endif else ltem[njj-1] = ''
				ftem = ftem(where(inds ge 0))
				ftem = [ftem[0:jl],ltem,ftem[jl+1:*]]
			endif
		endif
		sarr[slin[i]:slin[i+1]-1] = ftem
	endfor

	midnam = Fnamparse(firnam,pat=pat,ext=ext) + '_fixed'
	secnam = File_get(pat + midnam + ext,/write,stat=wsta)
	if not wsta then message, 'Output file not selected!
	openw, unit, secnam, /get_lun
	printf, unit, sarr, format = '(a)'
	free_lun, unit
	print
	print, '	Saved ' + secnam
	print

	return
end