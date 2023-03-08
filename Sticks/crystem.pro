Pro crystem, tet, vof = vof, border= bor

	on_error, 1

	Box, [-100,300], [-150,200], tru='ylo', xmar=[3,3], ymar=[2,2], border=bor
	rtet = !dtor*tet

	lin = transpose([[-50,300], [0,0]])
	plots, lin, line=1, thi=2

	fir = transpose([[0,0],[0,vof/(2*cos(rtet))]])
	sec = transpose([[0,vof/(2*sin(rtet))],[vof/(2*cos(rtet))*[1,1]]])
	thi = transpose([[0,vof/(2*sin(rtet))],[0,vof/(2*cos(rtet))]])
	fir = Shape_trans(fir,rtet)
	sec = Shape_trans(sec,rtet)
	thi = Shape_trans(thi,rtet)
	plots, fir, line=2, thi=2
	plots, sec, line=2, thi=2
	plots, thi, thi=2

	Arro, from = thi[*,1]*[1,0] + [5,0], to=thi[*,1] + [5,0], size=1.5, /two
	Arc_mm, [70,0], cen=[0,0], ang = 2*rtet, thi=2
	Labels, [0,-40,190,40,200], [-20,70,150,5,70], $
	['A','B','C','2!7h!x!n','Off'], align=0.5,charsize=2, charthi=1.5
	Labels, [20,20],[-60,-90], $
	['AB = Off/(2*cos!7h!x!n)','BC = Off/(2*sin!7h!x!n)'], $
	charsize=2,charthi=1.5


	return
end

	
