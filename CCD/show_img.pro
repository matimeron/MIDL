Pro Show_img, img, last = las, bin = bin, rotate = rot, pixsize = pxs, $
	hdist = hdst, center = cnt, dth = dth, bet = bet, alp = alp, $
	lam = lam, ene = ene, qvals = qvl, min = lo, max = hi, log = log, $
	radians = rad

	common misc_img, exs, linf, binf, bini
	on_error, 1

	if Type(exs) eq 0 then exs = 0
	if exs eq 0 then begin
		linf = ''
		binf = 2l
		bini = 1l
		exs = 1
	endif

	siz = size(img)
	case siz[0] of
		0	:	begin
					binf = Default(bin,binf,/dtyp)
					wbin=binf
					if not keyword_set(las) then begin
						wimg = CCD_img_read(bin= binf,img_inf=dum)
						linf = dum
					endif else wimg = CCD_img_read(fil=linf.fnam,bin=binf)
				end
		2	:	begin
					bini = 2l^(floor(alog(Default(bin,bini) > 1)/alog(2)))
					wbin=bini
					if bini eq 1 then wimg = ulong(img) $
					else wimg= ulong(rebin(bini^2*img,siz[1]/bini,siz[2]/bini))
				end
		else:	message, 'Not an image!'
	endcase
	if keyword_set(rot) then wimg = rotate(wimg,3)
	siz = size(wimg)
	x = lindgen(siz[1])
	y = lindgen(siz[2])

	if n_elements(cnt) ne 0 then begin

		cxy = CCD_coo_conv(x, y, pixs=pxs*wbin, hdist= hdst, cent= cnt/wbin, $
		dth = dth, bet = bet, alp = alp, lam = lam, ene = ene, qvals = qvl, $
		radians = rad)
		cx = reform(cxy[0,*,*])
		cy = reform(cxy[1,*,*])
		orx = Sign(cx[siz[1]-1,siz[1]-1] - cx[0,0])
		ory = Sign(cy[siz[2]-1,siz[2]-1] - cy[0,0])
		triangulate, cx, cy, tri
		x = Make_grid([min(cx,max=max),max],siz[1])
		y = Make_grid([min(cy,max=max),max],siz[2])
		if orx lt 0 then x = reverse(x)
		if ory lt 0 then y = reverse(y)
		wimg = FPU_fix(trigrid(cx,cy,wimg,tri, xout=x,yout=y)) > 0

		if keyword_set(qvl) then begin
			unit = ' (!sA!r!e !m% -1!x!n)'
			xtit = 'Q!dxy!n' + unit
			ytit = 'Q!dz!n' + unit
		endif else begin
			unit = ' (deg.)'
			xtit = 'Dth' + unit
			ytit = 'Beta' + unit
		endelse
	endif else begin
		unit = ' (pixels)'
		xtit = 'x' + unit
		ytit = 'y' + unit
	endelse

	if not keyword_set(log) then begin
		imax = max(wimg)
		ilo = (0 > Default(lo,0) < 1)*imax
		ihi = (0 > Default(hi,1) < 1)*imax
		wimg = ilo > wimg < ihi
	endif else wimg = alog(1 + wimg)

	lpad = 48l
	rpad = 24l
	window, /free, xsiz=siz[1] + lpad + rpad, ysiz = siz[2] + lpad + rpad
	plvar_keep, act = 'sav'
	erase
	device, deco = 0
	tvscl, wimg, lpad, lpad
	xrn = [x[0],x[siz[1]-1]]
	yrn = [y[0],y[siz[2]-1]]
	if !order then yrn = reverse(yrn)
	plot, x, y, xran= xrn, yran= yrn, /nodata, /noerase, /device, $
	xstyle = 9, ystyle = 9, ticklen = -0.01, xtit = xtit, ytit = ytit, $
	position = lpad + [0,0,siz[1]-1,siz[2]-1]

	cwid = 2*rpad/3
	carr = bytarr(cwid,siz[2])
	fill = reverse(256l*lindgen(siz[2])/siz[2])
	for i = 0l, cwid - 1 do carr[i,*] = fill
	tvscl, carr, lpad + siz[1] + rpad/3, lpad
	plvar_keep, act = 'res'

	return
end