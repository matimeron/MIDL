Function File_ring_scan, imginf, radius=rad, range = ran, degrees = deg, $
	step= stp, nsteps= nst, normalize = nrm, show_progress = sho, $
	output = out, nozero = noz, write = wri, _extra = _e

	on_error, 1

	img = ImgTem(imginf.read_inf.file,flip=imginf.read_inf.flip,$
			info = rdinf, _extra=_e)

	scinf = imginf.scan_inf
	scale = rdinf.dims/scinf.dims
	if not Arreq(scale,[1,1]) then if scale[0] eq scale[1] $
	then scinf.cent = scinf.cent*scale else message, 'Scaling problems'
	wimginf = {imgsym, read_inf: rdinf, scan_inf: scinf}

	res = Ring_scan(img, imgstruct= imginf.scan_inf, radius=rad, degrees=deg, $
		range= ran, step= stp, nsteps= nst, normalize= nrm, show=sho, extra=_e)

	if How_many(fir='out',sec='wri') gt 0 then begin
		n = Strparse_mm(wimginf.read_inf.file,'\',lis)
		nn = Strparse_mm(lis[n],'.',llis)
		ifil = llis[0]
		if keyword_set(deg) then auni = '(deg.)' else auni = '(rad.)'
		if wimginf.read_inf.zoom eq 1 then runi = '(pix.)' else $
		runi ='(pix./'+ string(wimginf.read_inf.zoom,form='(i1,")")')
		ahead = 'Angle ' + auni
		tit = 'Angular Scan'
		if keyword_set(nrm) then tit = 'Normalized ' + tit
		if wimginf.read_inf.lin then tit = tit + ' (lin. image)' $
		else tit = tit + ' (log. image)'
		case (n_elements(rad) <2) of
			0	:	tit= tit+', full rad. range'
			1	:	tit= tit+', rad. range of'+strcompress(string([0,rad],$
					form='(" [ ",f8.2,",",f8.2," ] ")')) + runi
			2	:	tit= tit+', rad. range of'+strcompress(string(rad[0:1],$
					form='(" [ ",f8.2,",",f8.2," ] ")')) + runi
		endcase
		tit = tit + '  ;  ' + ifil
		if keyword_set(out) then begin
			comst = 'plot, reform(res[0,*]), reform(res[1,*]),' + $
				'title = tit, xtit =ahead, ytit="Amplitude",xsty=1'
			if keyword_set(noz) then comst = comst + ',/ynoz'
			output, comst, sub= 'res, tit, ahead', res, tit, ahead
		endif
		if keyword_set(wri) then begin
			if Type(wri) eq 7 then ofil = wri else ofil = ifil + '_ang.txt'
			wascii,res,ofil,tit = tit, head = [ahead,'Amplitude']
		endif
	endif

	return, res
end