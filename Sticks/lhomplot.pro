Pro Lhomplot, border = bor

	on_error, 1

	thi = 3
	Box, [0,10], [0,10], tru = 'ylo', bor=bor

	plots, [0,8], [2,2], thi=thi, line=2
	plots, [0,1], [2,2], thi=thi
	plots, [1,3], [2,5], thi=thi, line=2
	plots, [3,8], [5,5], thi=thi, line=2
	fra = .6
	plots, [1,1 + 2*fra], [2,2 + 3*fra], thi = thi
	plots, [1 + 2*fra,8], [2 + 3*fra,5], thi = thi
	
	Arc_mm, [1 + 0.5*2*fra,2 + 0.5*3*fra], cen = [1,2], ang = -55, /deg, thi=2
	Arc_mm, [1 + 1.5*2*fra,2 + 1.5*3*fra], cen = [1+2*fra,2+3*fra], $
	ang = -45, /deg, thi=2
	Arc_mm, [4,5], cen = [8,5], ang=12, /deg, thi=2

	Arro, from=[1,1.5], to = [8,1.5], /two, size=1.5
	Arro, from=[8.5,2], to = [8.5,5], /two, size=1.5

	Labels, [1.7, 2.8, 3.7, 9.1]-1, $
			1+[1.1, 2.8, 4.1, 4.1], $
			['A', 'B', 'C', 'D'], charsize =2, charthi=2
	Labels, [2.4,           3.62         , 5.1]-1, $
			1+[1.2,           3.15         , 3.63], $
			['2!7h!x!dC!n', '2!7h!x!dW!n', '2(!7h!x!dC!n-!7h!x!dW!n)'], $
			charsize=1.5, charthi=1.5
	Labels, [4.5,8.7],[1.1,3.5],['len','off'], charsize=1.5,charthi=1.5

	return
end