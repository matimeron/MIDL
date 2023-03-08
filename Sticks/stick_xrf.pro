Pro Stick_xrf, alpha = alp, border = bord, rough = rof, _extra = _e

	on_error, 1

	glim = [-1,1]
	icen = [1.,0]
	ocen = [-1.,0]

	ccen = 2*icen	
	rectan, xli = 0.15*glim, yli = 0.03*glim, /no_show, shap = crys
	crys = Shape_trans(crys,0,1,ccen + [0,0.03])

	tcen = [0.,0]
	rectan, xli = 0.3*glim, yli = 0.08*glim, /no_show, shap = otrof
	otrof = Shape_trans(otrof,0,1,tcen + [0,-0.085])
	if keyword_set(rof) then begin
		pha = randomu(s,1)
		rectan,xli=0.25*glim,yli=0.05*glim,/no_show,xwig=[5.,.2,pha],shap= itrof
	endif else rectan,xli=0.25*glim,yli=0.05*glim,/no_show,rad=0.01,shap= itrof
	itrof = Shape_trans(itrof,0,1,tcen + [0,-0.05])

	dcen = 1.5*ocen
	rectan, xli = 0.05*glim, yli = 0.15*glim, /no_show, shap = det
	det = Shape_trans(det,0,1,dcen + [-0.05,0.1])

	sgen = Shape_close([[0,0],[0.03,0.1],[-0.03,0.1]])
	s1t = Shape_trans(sgen,0,1,icen + [0,0.01])
	s1b = Shape_trans(sgen,!pi,1,icen - [0,0.01])
	s2t = Shape_trans(sgen,0,1,ocen + [0.3,0.01])
	s2b = Shape_trans(sgen,!pi,1,ocen + [0.3,-0.01])
	s3t = Shape_trans(sgen,0,1,ocen + [-0.2,0.01])
	s3b = Shape_trans(sgen,!pi,1,ocen + [-0.2,-0.01])

	walp = !dtor*Default(alp,0.,/dtyp)
	wccen = ccen
	wicen = [1,-tan(walp)]
	wtcen = [0,-2*tan(walp)]
	wocen = [-1,-tan(walp)]
	wdcen = wocen + 0.5*[-cos(walp),sin(walp)]

	crys = Shape_trans(crys,walp/2,1,cen=ccen)
	s1t = Shape_trans(s1t,walp,1,wicen-icen,cen=icen)
	s1b = Shape_trans(s1b,walp,1,wicen-icen,cen=icen)	
	otrof = Shape_trans(otrof,0,1,wtcen-tcen)
	itrof = Shape_trans(itrof,0,1,wtcen-tcen)
	s2t = Shape_trans(s2t,-walp,1,wocen-ocen,cen=ocen)
	s2b = Shape_trans(s2b,-walp,1,wocen-ocen,cen=ocen)	
	s3t = Shape_trans(s3t,-walp,1,wocen-ocen,cen=ocen)
	s3b = Shape_trans(s3b,-walp,1,wocen-ocen,cen=ocen)	
	det = Shape_trans(det,-walp,1,wocen-ocen,cen=ocen)

	Plvar_keep, act = 'save'
	!x.margin = [5,5]
	!y.margin = [4,4]
	Box, [-2.,2.5], [-2.,1], truasp = 'ylo', bord= bord

	plots, [[3,0],[wccen],[wtcen],[wdcen]], thi = 4, col = !pcol.cyan
	plots, [[3,0],[wccen],[wicen]], thi = 6, col = !pcol.cyan
	polyfill, crys, color = !pcol.dgreen
	polyfill, s1t, color = !pcol.purple
	polyfill, s1b, color = !pcol.purple
	polyfill, otrof, color = !pcol.black
	polyfill, itrof, color = !pcol.lblue
	polyfill, s2t, color = !pcol.purple
	polyfill, s2b, color = !pcol.purple
	polyfill, s3t, color = !pcol.purple
	polyfill, s3b, color = !pcol.purple
	Polyfill, det, color = !pcol.dblue

	plots, 0.8*[-1,1], wtcen[1]*[1,1], line=2, col=!pcol.lblue
	Arc_mm, wtcen + [0.6,0], cen = wtcen, ang = walp, col=!pcol.lblue
	Arc_mm, wtcen + [-0.6,0], cen = wtcen, ang = -walp, col=!pcol.lblue
	if !radeg*walp ge 15 then Labels, [-0.5,0.5], replicate(wtcen[1]+ 0.02,2),$
	replicate('!4a!x',2), align = 0.5, _extra = _e

	Plvar_keep, act = 'rest'

	return
end
