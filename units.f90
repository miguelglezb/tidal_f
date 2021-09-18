! Canonical code constants

subroutine units(g, t_units, r_units, vel_units, m_units, pi, ener_units, spec_ener_units, &
                    ang_mom_units, spec_ang_mom_units)
    implicit none
    double precision, intent(out) :: g, t_units, r_units, vel_units, m_units, pi
    double precision, intent(out) :: ener_units, spec_ener_units, ang_mom_units
    double precision, intent(out) :: spec_ang_mom_units

    g = 1d0                                               ! Canonical grav constant
    pi = 4d0*datan(1d0)                                   ! Pi
    m_units = 5.97d27                                     ! Canonical mass units (Earth mass)     
    r_units = 6.957d10                                    ! Canonical radius units (Solar radius)
    t_units = sqrt(r_units**3/(6.674d-8*m_units))         ! Canonical time units (9.19E5 s or ~11 days) 
    vel_units = r_units/t_units                           
    ener_units = m_units*vel_units**2
    spec_ener_units = vel_units**2
    ang_mom_units = m_units*vel_units*r_units
    spec_ang_mom_units = vel_units*r_units
end subroutine units
