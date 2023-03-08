Function BC_efun, x, par

	on_error, 1

	r = par[0]*par[1]/(2*par[2])/sqrt((par[0]*sin(x))^2 + (par[1]*cos(x))^2)

	return, r*Poleval(r,par[3:5])
end