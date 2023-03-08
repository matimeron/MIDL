Pro Det_arm, vel, omega = omg, gap = gap, distance = dst, show = sho

	on_error, 1

	x0 = 300.
	y0 = -110.

	r = sqrt(x0^2 + y0^2)
	psi0 = atan(y0,x0)
	womg = !dtor*Default(omg,!radeg*vel/x0)
	wgap = Default(gap,0.)
	tim = Make_grid([0,1.*dst/vel],1,/step)

	if keyword_set(sho) then begin
		print
		print, !radeg*womg, form = '("angular velocity = ",f7.4," deg./sec")'
		print
	endif
	plot, tim, wgap + vel*tim - 2*r*sin(womg*tim/2)*cos(psi0-womg*tim/2)

	return
end