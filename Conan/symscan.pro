Function Symscan, img, file = fname, readstruct = rst, degrees = deg, $
	arcsize = arc, center = cnt, range = ran, step = stp, round = rnd, $
	verify = ver, show = sho, _extra = _e

	on_error, 1
	pmax = 16

	posib = ['result','progress','all']
	if keyword_set(sho) then begin
		if Type(sho) eq 7 then shofl = Strmatch_mm(sho,posib,3) > 0 $
		else shofl = 0
	endif else shofl = -1

	if keyword_set(deg) then amul = !dtor else amul = 1.
	if n_elements(arc) ne 0 then warc = amul*arc else warc = !pi/2

	if Type(img) eq 0 then begin
		stat = One_of(fname,rst) > 0
		if stat then ifil = rst.file else ifil = File_get(fname,stat=stat)
		if stat then img = ImgTem(ifil,info = rdinf,_extra=_e) $
		else message, 'No such file!'
	endif else rdinf = {imgread}
	dims = (size(img))[1:2]

	if n_elements(cnt) eq 0 then begin
		cnt = Pick_point(img,que='Select center',verify=ver)
		print, 'Center = ', cnt
	endif

	nran = n_elements(ran)
	if nran lt 2 then begin
		if nran eq 0 then begin
			sloc = Pick_point(img,que='Select scan start',verify=ver)
			wran = floor(sqrt(total((sloc - cnt)^2)))
			print, 'Start radius = ', wran[0]
			nran = nran + 1
		endif
		if nran eq 1 then begin
			sloc = Pick_point(img,que='Select scan end',verify=ver)
			wran = [Default(ran,wran),ceil(sqrt(total((sloc - cnt)^2)))]
			print, 'End radius = ', wran[1]
			nran = nran + 1
		endif
	endif else wran = ran
	wran = [min(wran,max=max),max]
	dwran = wran[1] - wran[0]
	if keyword_set(rnd) then wran = round(wran)

	sang = !pi/2*findgen(4)
	wedg = warc/2*[-1,1]

	for i = 0, 3 do begin
		tem = Wedge_scan(img,cent=cnt,angle=sang[i]+wedg,rang=wran,step=stp,$
				/norm,scan=npo,show = shofl gt 0)
		if i eq 0 then begin
			rval = tem[0,*]
			spec = fltarr(4,npo)
		endif
		spec[i,*] = tem[1,*]
	endfor

	pstat = intarr(4)
	rav = fltarr(4)
	for i = 0, 3 do begin
		ploc = Peak_main(rval,reform(spec[i,*]),stat=stat,_extra=_e)
		pstat[i] = stat[0]
		if stat[0] then rav[i] = ploc
	endfor

	if Arreq(pstat,[1,1,1,1]) then begin
		cent = cnt + 0.5*[rav[0]-rav[2],rav[1]-rav[3]]
		ax = 0.5*[rav[0]+rav[2],rav[1]+rav[3]]
		if ax[0] lt ax[1] then begin
			rot = !pi/2
			ax = reverse(ax)
		endif else rot = 0.
		exc = sqrt(1 - (ax[1]/ax[0])^2)

		if not (shofl mod 2) then begin
			ringoff = [4,5]
			wimg = Byte_img(img, size=siz)
			dum = Consec(size=siz,cent=cent,rad=1,/new)
			ldum = Consec(size=siz,rad=ax[0] - ringoff,excen=exc,rot=rot)
			hdum = Consec(size=siz,rad=ax[0]+ringoff)
			dum = [dum,ldum,hdum]
			wimg[dum] = wimg[dum] + 128b
			if !d.window eq (-1) then wset
			wshow
			tvscl, wimg
		endif
		scinf = {imgscan, dims, 1, cent, exc, rot, 0}
	endif else begin
		scinf = {imgscan}
		message, "Can't determine center!" , /con
	endelse

	res = {imgsym, read_inf: rdinf, scan_inf: scinf}
	return, res
end
