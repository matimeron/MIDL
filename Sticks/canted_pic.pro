Pro Canted_pic, ang, border= bor, _extra= _e

	on_error, 1

	Box, [-80,40], [-50,50], tru='xhi', xmar=[3,3], ymar=[2,2], border=bor
	ang = 35
	Circle_mm, cent=[0,0], rad = 15, /fill, col = !pcol.lblue, shap=cir
	Rectan, xli = [-90,5], yli=[-1.25,1.25], /no_show, shap = beam
	Rectan, xli = [-90,50], yli=28+[-1.25,1.25], /no_show, shap = sbeam
	Rectan, xli = [-20,20], yli = [-10,0], /no_sho, shap=crys
	Rectan, xli = [-35,35], yli = [-10,0], /no_show, shap=lcrys
	fcrys = Shape_trans(crys,ang,/deg)
	scrys = Shape_trans(crys,90,/deg)
	flcrys = Shape_trans(lcrys,ang,/deg)
	polyfill, beam, col = !pcol.red, noclip=0
	polyfill, sbeam, col= !pcol.lred, noclip=0
	polyfill, flcrys, col=!rainbow[7]
;	polyfill, fcrys, col = !pcol.black
;	plots, scrys, line=2, thi=4

	Arro, from = [-70,0], to = [-70,28], siz=1.5, thi=2, /two 
	Arro, from = [-45,1.25], to = [-45,26.75], siz=1.5, thi=2, /two
	Arro, from = [28.6,20], to = [28.6,26.75], siz=1.5, thi=2, /two

	Labels, [-80,-80,-69,-44,13], [-5,30,13,13,22], $
	['Inboard Beam','Outboard Beam','28 mm','25 mm','5 mm'],$
	charsize =2, charthi=2

	return
end