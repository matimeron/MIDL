Function File_wedge_scan, imginf, angle = ang, range = ran, degrees = deg, $
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

	res = Wedge_scan(img, imgstruct=wimginf.scan_inf, angle=ang, degrees=deg, $
		range = ran, step=stp, nsteps=nst, normalize=nrm, show=sho, extra=_e)

	if How_many(fir='out',sec='wri') gt 0 then begin
		n = Strparse_mm(wimginf.read_inf.file,'\',lis)
		nn = Strparse_mm(lis[n],'.',llis)
		ifil = llis[0]
		if keyword_set(deg) then auni = '(deg.)' else auni = '(rad.)'
		if wimginf.read_inf.zoom eq 1 then runi = '(pix.)' else $
		runi ='(pix./'+ string(wimginf.read_inf.zoom,form='(i1,")")')
		rhead = 'Radius ' + runi
		tit = 'Radial Scan'
		if keyword_set(nrm) then tit = 'Normalized ' + tit
		if wimginf.read_inf.lin then tit = tit + ' (lin. image)' $
		else tit = tit + ' (log. image)'
		case (n_elements(ang) <2) of
			0	:	tit= tit+', full ang. range'
			1	:	tit= tit+', ang. range of'+strcompress(string([0,ang],$
					form='(" [ ",f8.2,",",f8.2," ] ")')) + auni
			2	:	begin
						adif = abs(ang[1]-ang[0])
						if keyword_set(deg) then adif = !dtor*adif
						if adif/(2*!pi) + Toler() lt 1 then $
							tit= tit + ', ang. range of' + $
							strcompress(string(ang[0:1],$
							form='(" [ ",f8.2,",",f8.2," ] ")')) + auni else $
							tit= tit+', full ang. range'
					end
		endcase
		tit = tit + '  ;  ' + ifil
		if keyword_set(out) then begin
			comst = 'plot, reform(res[0,*]), reform(res[1,*]),' + $
				'title = tit, xtit =rhead, ytit="Amplitude",xsty=1'
			if keyword_set(noz) then comst = comst + ',/ynoz'
			output, comst, sub= 'res, tit, rhead', res, tit, rhead
		endif
		if keyword_set(wri) then begin
			if Type(wri) eq 7 then ofil = wri else ofil = ifil + '_rad.txt'
			wascii,res,ofil,tit = tit, head = [rhead,'Amplitude']
		endif
	endif

	return, res
end