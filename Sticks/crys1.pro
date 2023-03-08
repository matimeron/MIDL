Pro Crys1, ang, coff, radians = rad, border = bor

	on_error, 1

	if keyword_set(rad) then mult = 1 else mult = !dtor
	wang = mult*Default(ang,0)
	coff = Default(coff,0.)
	cofl = coff ne 0

	Box, [-1,1], [-1,1], tru='ycen', xmar=[3,3], ymar=[2,2], border=bor

	Rectan, xli = [0,0.7], yli=[-1,1], /no_sho, shape = scr
	rcr = Shape_trans(scr,wang,1,cen=[coff,0])

	Plvar_keep, act='sav'

;	plots, scr, noclip=0, line = 2
	polyfill, rcr, col=!pcol.lblue, noclip=0
	plots, rcr, col = !pcol.dblue, thi = 2, noclip=0

	cen = [0.,coff]
	ofcen = [0.,0]
	rfcen = coff*[1-cos(wang),-sin(wang)]
	iloc = coff*[0,-tan(wang/2)]

	locs = coff*[[1,0],[0,0],[1-cos(wang),-sin(wang)],[0,-tan(wang/2)]]
	psym = [7,8,8,8]
	if cofl then n = 3 else n = 0
	for i = 0, n do plots, locs[*,i], psym = psym[i], symsize=2, thi=2

	Arro, from =[0,-1],to=locs[*,3],size=2,thi=2,col=!pcol.dred,noclip=0
	Arro, from=locs[*,3], to=locs[*,3] + 0.7*[-sin(2*wang),cos(2*wang)], $
		size=2,thi=2,col=!pcol.dred,noclip=0

	if cofl then Labels, locs[0,*]+0.07*[1,-1,-1,1], locs[1,*]-0.02, $
	['C','A',"A'",'B'], charsize=1.8,charthi=1.6,align=0.5 $
	else Labels, locs[0,0]+0.07*[1], locs[1,0]-0.02, $
	['C'], charsize=1.8,charthi=1.6,align=0.5

	Plvar_keep, act='res'

	return
end
