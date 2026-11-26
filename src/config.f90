! Mesh sizes of the run (ncells, nedges, nvertices and nc1/ne1/nv1 = size + 1 for the zero ghost slot), set once from the
! mesh file by set_mesh_sizes, and the fixed array bounds (max_levels, maxedges, vertex_degree, max_adv).
module config
 use run_settings, only: dt
 use levels, only: nz, nz1, set_levels, reset_levels
 implicit none
 private :: set_levels, reset_levels
 ! The mesh sizes: set once per run from the mesh file (set_mesh_sizes); until then placeholder sizes (default_*: the athens_3km
 ! case of cases/athens_3km, the size tools/gen_memory.py also uses).
 integer, parameter, private :: default_cells=46251, default_edges=139361, default_vertices=93111
 integer, protected, save :: ncells=default_cells, nedges=default_edges, nvertices=default_vertices
 integer, protected, save :: nc1=default_cells+1, ne1=default_edges+1, nv1=default_vertices+1
 logical, private, save :: mesh_sizes_set = .false.
 integer, parameter :: max_levels=60
 integer, parameter :: maxedges=8
 integer, parameter :: vertex_degree=3, max_adv=15
contains
 subroutine set_mesh_sizes(cells, edges, vertices, level_count)
 integer, intent(in) :: cells, edges, vertices, level_count
 if (mesh_sizes_set) then
  if (cells /= ncells .or. edges /= nedges .or. vertices /= nvertices .or. level_count /= nz) &
   error stop 'mesh sizes are set once per run'
  return
 end if
 if (min(cells, edges, vertices, level_count) < 1) error stop 'mesh sizes must be positive'
 if (level_count > max_levels) error stop 'mesh levels exceed max_levels'
 ! The setter is compiled with card flags and updates the device copy.
 call set_levels(level_count)
 ncells = cells
 nedges = edges
 nvertices = vertices
 nc1 = cells + 1
 ne1 = edges + 1
 nv1 = vertices + 1
 mesh_sizes_set = .true.
 end subroutine

 ! Tests reading several independent meshes in one process only.
 subroutine clear_mesh_sizes()
 mesh_sizes_set = .false.
 call reset_levels()
 ncells = default_cells
 nedges = default_edges
 nvertices = default_vertices
 nc1 = ncells + 1
 ne1 = nedges + 1
 nv1 = nvertices + 1
 end subroutine
end module
