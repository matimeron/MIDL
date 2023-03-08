Pro Bounce_u, angran, step= stp, crystal= crs, index= find, $
	water= wat, glass= gls, layout= lay, next= nxt, _extra= _e

	common fspdat, exs, erange, fspc
	on_error, 1

	if exs then begin
		crs = Default(crs,'ge',/dtyp)
		ene = Make_grid(erange,stp,/step)
		fang = Bragg_angle(ene=ene,crys=crs,ind=find,dar=dar)
		bang = 2*fang
		val = where (bang ge angran[0] and bang le angran[1],nval)
		if nval gt 0 then begin
			ene = ene[val]
			bang = bang[val]
			wdar = 1e6*!dtor*dar[val]
		endif else message, 'No angle/energy values in range!
		ref = Ref_curve(0,ene=ene,crys=crs,ind=find)

		aabc = Abs_coeff(ene,ele=['n','o','ar'],wei=[78,21,1.],/form,den=1.2e-3)
		gabc = Abs_coeff(ene,ele=['si','o'],wei=[1,2],/form,den=2.5)
		wabc = Abs_coeff(ene,ele=['h','o'],wei=[2,1],/form,den=1)
		air = 50
		abfac = exp(-(air*aabc + 0.1*(gls*gabc+wat*wabc))/sin(!dtor*bang))

		asiz = Poleval(ene,[55.,-1])
		anfac = 1/(asiz*sqrt(1/asiz^2 + 1/wdar^2))
		atten = ref*abfac*anfac
		rflu = Splin_eval(ene,fspc)
		tflu = atten*rflu

		bsiz = 32
		dxysi = [20,16]*bsiz
		posib = ['sin','hor','ver']
		wlay = Strmatch_mm(lay,posib,1) > 0
		case wlay of
			0	:	begin
						xysi = dxysi
						phi = 0.80
						ylab = 0.96
						pp = [0,0,1,phi]
						aln = 0.40
					end
			1	:	begin
						xysi = [30,12]*bsiz
						phi = 0.92
						ylab = 0.95
						pp = [[0,0,0.5,phi],[0.5,0,1,phi]]
						aln = 0.45
					end
			2	:	begin
						xysi = [16,24]*bsiz
						phi = 0.92
						ylab = 0.97
						pp = [[0,phi/2,1,phi],[0,0,1,phi/2]]
						aln = 0.40
					end
		endcase
		xlab = 0.5
		crnam = strmid(crs,0,2)
		ftit = crnam + string(find,form='("(",3i1,") up bounce")')
		stit = string(wat,form='("Water ",f3.1,"mm,")') + $
		string(gls,form='(t2,"Glass ",f3.1,"mm")')
		if wlay eq 1 then tit = ftit + '.  ' + stit $
		else tit = ftit + '!c!b' + stit + '!n'
		
		Plvar_keep, act = 'sav'
		dwin = (!d.window + keyword_set(nxt)) > 0
		window, dwin, xsi = xysi[0], ysi = xysi[1]

		if wlay eq 0 then begin
			!x.margin = [11,4]
			!p.region = pp
			plot, bang, tflu, thi=2, ystyle=2, xtick_get=atics, $
			xtit='Bounce Angle (deg)', ytit='Flux (ph/s)', _extra=_e
			tsp = Splin_coeffs(bang,ene)
			etics = Splin_eval(atics,tsp)
			etics = string(etics,form='(f5.1)')
			axis, xaxis=1, xticks=n_elements(etics)-1,xtickn=etics, $
			xtit='Energy (keV)!c', _extra=_e
		endif else begin
			!x.margin = [11,3]
			window, dwin, xsi = xysi[0], ysi = xysi[1]
			!p.region = pp[*,0]
			plot, bang,tflu,thi=2,xtit='Bounce Angle (deg)',ytit='Flux (ph/s)',$
			_extra=_e
			!p.region = pp[*,1]
			plot,ene, tflu, thi=2, xtit='Energy (keV)', ytit='Flux (ph/s)', $
			/noerase, _extra=_e
		endelse

		dum = Wherinstruct('chars',_e)
		if dum ge 0 then _e.(dum) = 1.2*_e.(dum)
		Labels, xlab,ylab,tit, align= aln, /normal, charsize=1.5, _extra=_e
		Plvar_keep, act = 'res'

	endif else message, 'Not initialized!'

	return
end