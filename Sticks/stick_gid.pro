Pro Stick_gid, phi = phi, border = bord, _extra = _e

	on_error, 1

	glim = [-1,1]
	icen = [1.,0]
	ocen = [-1.,0]

	ccen = 2*icen	
	rectan, xli = 0.15*glim, yli = 0.10*glim, /no_show, shap = crys
	crys = Shape_trans(crys,0,1,ccen)

	tcen = [0.,0]
	rectan, xli = 0.3*glim, yli = 0.2*glim, /no_show, shap = otrof
	rectan,xli=0.25*glim,yli=0.15*glim,/no_show,rad=0.01,shap= itrof

	dcen = 1.5*ocen
	rectan, xli = 0.05*glim, yli = 0.15*glim, /no_show, shap = det
	det = Shape_trans(det,0,1,dcen)

	sgen = Shape_close([[0,0],[0.03,0.1],[-0.03,0.1]])
	s1t = Shape_trans(sgen,0,1,icen + [0,0.01])
	s1b = Shape_trans(sgen,!pi,1,icen - [0,0.01])
	s2t = Shape_trans(sgen,0,1,ocen + [0.3,0.01])
	s2b = Shape_trans(sgen,!pi,1,ocen + [0.3,-0.01])
	s3t = Shape_trans(sgen,0,1,ocen + [-0.2,0.01])
	s3b = Shape_trans(sgen,!pi,1,ocen + [-0.2,-0.01])

	wphi = !dtor*Default(phi,0.,/dtyp)
	wccen = ccen
	wicen = icen
	wtcen = tcen
	wocen = [-cos(wphi),-sin(wphi)]
	wdcen = 1.5*wocen

	s2t = Shape_trans(s2t,wphi,cen=wtcen)
	s2b = Shape_trans(s2b,wphi,cen=wtcen)	
	s3t = Shape_trans(s3t,wphi,cen=wtcen)
	s3b = Shape_trans(s3b,wphi,cen=wtcen)	
	det = Shape_trans(det,wphi,cen=wtcen)

	Plvar_keep, act = 'save'
	!x.margin = [5,5]
	!y.margin = [4,4]
	Box, [-2.,2.5], [-2.,1], truasp = 'ylo', bord= bord

	polyfill, otrof, color = !pcol.black
	polyfill, itrof, color = !pcol.lblue
	plots, [[wtcen - [0.25,0]],[wtcen + [0.25,0]]], thi = 8, col = !pcol.blue
	plots, [[3,0],[wccen],[wtcen],[-0.7,0]], thi = 4, col = !pcol.cyan
	if wphi lt atan(0.01/0.7) then $
		plots , [[wtcen],[dcen]], col = !pcol.cyan,thi = 4 $
	else if wphi gt atan(0.1/0.7) then $
		plots , [[wtcen],[-3,0]], col = !pcol.cyan, thi = 4
	plots, [[wtcen],[wdcen]], thi = 4, line = 2, col = !pcol.cyan
	polyfill, crys, color = !pcol.dgreen
	polyfill, s1t, color = !pcol.purple
	polyfill, s1b, color = !pcol.purple
	polyfill, s2t, color = !pcol.purple
	polyfill, s2b, color = !pcol.purple
	polyfill, s3t, color = !pcol.purple
	polyfill, s3b, color = !pcol.purple
	Polyfill, det, color = !pcol.dblue

	Arc_mm, tcen + [-1.,0], cen=tcen, ang=wphi, col=!pcol.lblue
	if !radeg*wphi ge 15 then Labels, -0.9, -0.15, $
	replicate('!4u!x',2), align = 0.5, _extra = _e

	Plvar_keep, act = 'rest'

	return
end