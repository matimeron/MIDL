Pro wcrysf, border = bor

	on_error, 1
	box, [-1,1], [0,2], truasp='ylo', border = bor, xmar= [3,3], ymar= [2,2]

	for i = 0, 50 do plots, [-1,1], [0.02*i,0.02*i],  col=!pcol.blue, thi=2
	x = make_grid([-1,1],.005,/step)
	oplot, x, 1 + 0.04*sin(3*x), thi=28, col=!pcol.white
	
	
	return
end