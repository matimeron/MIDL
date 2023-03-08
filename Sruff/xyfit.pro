Function XYfit, farr, xarr, yarr, window = win, funcfirst = ffr, slice = sli, $
	gauss = gus, quartic = qua, weight = weipow, positive_only = poo, $
	peak_keep = pek, show_diff = shdif, show_fit = shfit, contour = cont, $
	split_head = sph, rel_error = rel, integrals = ing, _extra = _e

	on_error, 1

	ftyp = Type(farr)
	fsiz = size(farr)
	qfl = Sign(([Default(qua,0),0,0])(0:2)) > 0
	coarr = [[1,1,1],qfl]
	gusfl = keyword_set(gus)
	pekfl = keyword_set(pek)
	sphfl = keyword_set(sph)

	case n_params() of
		1	:	begin
					if fsiz[0] eq 3 then begin
						nl = fsiz[1]
						if nl ge 3 then begin
							if nl gt 3 then begin
								if Isnum(sli) then sl = sli $
								else message, 'Missing slice #!'
							endif else sl = Default(sli,0l)
							if sl lt 0 or sl gt (nl-3) then $
							message, 'Slice out of range!'
						endif else message, 'Wrong input dimension!'
						if keyword_set(ffr) then begin
							xvl = reform(farr[nl-2,*,*])
							yvl = reform(farr[nl-1,*,*])
							fvl = reform(farr[sl,*,*])
						endif else begin
							xvl = reform(farr[0,*,*])
							yvl = reform(farr[1,*,*])
							fvl = reform(farr[sl+2,*,*])
						endelse
					endif else message, 'Wrong input dimension!'
				end
		3	:	begin
					if fsiz[0] eq 2 then begin
						fvl = farr
						xsiz = size(xarr)
						ysiz = size(yarr)
						if Arreq([xsiz,ysiz],[fsiz,fsiz]) then begin
							xvl = xarr
							yvl = yarr
						endif else begin
							if xsiz[0] eq 1 and ysiz[0] eq 1 and $
							Arreq([xsiz[1],ysiz[1]],fsiz[1:2]) then begin
								xvl = Make_array(size= fsiz,type= fsiz[3]> 4)
								yvl = xvl
								for k = 0l, ysiz[1] - 1 do xvl[*,k] = xarr
								for k = 0l, xsiz[1] - 1 do yvl[k,*] = yarr
							endif else message, 'Wrong X and/or Y sizes!'
						endelse
					endif else message, 'Wrong F dimension!'
				end
		else:	message, 'Input data problems!'
	endcase

	x = reform(xvl[*,0])
	y = reform(yvl[0,*])
	sxy = [n_elements(x),n_elements(y)]

	if keyword_set(poo) then begin
		dum = where(Partot(fvl,2) gt 0, nzx)
		dum = where(Partot(fvl,1) gt 0, nzy)
		nzwin = [nzx,nzy]*dxy/2
		wwin = wwin < nzwin
	endif

	frame = Winframe(x,y,window=win,xygrid=wxy,spac=dxy,windex=qxy,fringe=fri)
	xco = Winsnip(sxy,qxy,x,/x_vec,/full)
	yco = Winsnip(sxy,qxy,y,/y_vec,/full)

	fvl = Winsnip(sxy,qxy,fvl,/full)
	wfvl = fvl
	if gusfl then wfvl = alog(wfvl > Toler(farr))
	if pekfl then begin
		pval = max(wfvl)
		wfvl = wfvl - pval
	endif

	glob = make_array(size=[3,6,(size(fvl))[1:*]],type=ftyp>5)
	if not pekfl then glob[0,*,*] = 1
	glob[1,*,*] = Winsnip(sxy,qxy,temporary(xvl),/full)
	glob[2,*,*] = Winsnip(sxy,qxy,temporary(yvl),/full)
	if qfl[0] gt 0 then glob[3,*,*] = glob[1,*,*]^2
	if qfl[1] gt 0 then glob[4,*,*] = glob[2,*,*]^2
	if qfl[2] gt 0 then glob[5,*,*] = glob[1,*,*]*glob[2,*,*]
	glob = glob^2

	if n_elements(weipow) ne 0 then begin
		wei = fvl^weipow
		wei = wei/Partot(wei,syme=qxy,symf=fri)
	endif else wei = 1.

	sysar = Make_array(6, 6, type = ftyp > 5)
	fvec = Make_array(6, type = ftyp > 5)

	for i = 0, 5 do begin
		fvec(i) = Partot(wei*reform(glob(i,*,*))*wfvl,syme=qxy,symf=fri)
		for j = 0, i do begin
			sysar(i,j) = $
			Partot(wei*reform(glob(i,*,*)*glob(j,*,*)),syme=qxy,symf=fri)
			sysar(j,i) = sysar(i,j)
		endfor
	endfor

	res = Solve_linsys(sysar,fvec,/svd)*coarr
	if pekfl then begin
		glob[0,*,*] = 1
		res[0] = pval
	endif

	fit = 0*fvl
	for k = 0, 5 do fit = fit + res[k]*reform(glob(k,*,*))
	if gusfl then fit = exp(fit)
	fit = Cast(fit,4,ftyp)
	res = Cast(res,4,ftyp)

	rel = sqrt(Partot(wei*(fvl-fit)^2,syme=qxy,symf=fri)/ $
	Partot(wei*fvl^2,syme=qxy,symf=fri))
	arel = dxy[0]*dxy[1]
	ing = arel*[Partot(fvl,syme=qxy,symf=fri),Partot(fit,syme=qxy,symf=fri)]

	if keyword_set(shdif) then begin
		if keyword_set(cont) then begin
			ftem = fvl-fit
			fmin = min(ftem,max=fmax)
			step = 10.^floor(alog10((fmax-fmin)>Toler(ftem)))
			repeat begin
				rfmin = step*floor(fmin/step)
				rfmax = step*ceil(fmax/step)
				ns = round((rfmax-rfmin+step)/step)
				if ns le 5 then step = step/2.
			endrep until ns gt 5
			levs = make_grid([rfmin,rfmax],ns)
			labs = 1 + intarr(ns)
			contour, ftem, xco, yco, /follow, levels=levs, c_labels=labs, /down
		endif else surface, fvl-fit, xco, yco
	endif

	if keyword_set(shfit) then begin
		xcn = (n_elements(xco) - [0,1])/2
		ycn = (n_elements(yco) - [0,1])/2

		sterm = ['','x!e2!n','y!e2!n','x!e4!n','y!e4!n','x!e2!ny!e2!n']
		sval = string(res,form='(g15.5)')
		for i = 1, 5 do if res[i] ge 0 then sval[i] = $
		' +' + strcompress(sval[i],/remove)
		sval[1:5] = sval[1:5] + sterm[1:5]
		dum = where(coarr eq 0, ndum)
		if ndum gt 0 then sval(dum) = ''
		if sphfl then begin
			scomb = strarr(2)
			scomb[0] = strcompress(string(sval[0:2],'(3a)'))
			scomb[1] = strcompress(string(sval[3:5],'(3a)'))
			sspac = string(replicate(32b,24))
			lspac = string(replicate(32b,24))
			if gusfl then head = ['Fit = exp(',lspac] + scomb + ['',')'] $
			else head = ['Fit = ',sspac] + scomb
			ylab = [.7,-.7]
			del = .05
		endif else begin
			scomb = strcompress(string(sval,'(6a)'))
			if gusfl then head = 'Fit = exp(' + scomb + ')' $
			else head = 'Fit = ' + scomb
			ylab = 0
			del = 0.
		endelse

		plvar_keep, act = 'sav'
		!p.multi = [0,1,3]
		!p.region = [0,.9-2*del,1,1]
		plot, [-1,1],[-1,1], /nodata, xstyle = 12, ystyle = 12
		labels, 0, ylab, head, align = .5 + 2*del, charsize = 1.0 + 4*del
		!p.region = [0,.45-del,1,.9-2*del]
		tem = [fvl(xcn[0],*)+fvl(xcn[1],*),fit(xcn[0],*)+fit(xcn[1],*)]/2
		plotot, yco, tem, thick=2, line = [0,2], xtit = 'Y (mm)', $
		title = 'Fit along the Y-axis', charsize = 2, _extra = _e
		!p.region = [0,0,1,.45-del]
		tem = [[fvl(*,ycn[0])+fvl(*,ycn[1])],[fit(*,ycn[0])+fit(*,ycn[1])]]/2
		plotot, xco, tem, thick=2, line = [0,2], xtit = 'X (mm)', $
		title = 'Fit along the X-axis', charsize = 2, _extra = _e
		plvar_keep, act = 'res'
	endif

	return, FPU_fix(res)
end