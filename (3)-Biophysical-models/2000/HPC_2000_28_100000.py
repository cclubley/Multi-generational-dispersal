from parcels import FieldSet, Field, ParticleSet, JITParticle, AdvectionRK4, ErrorCode, ParcelsRandom, Variable, VectorField, DiffusionUniformKh
from datetime import timedelta as delta
import numpy as np
from datetime import datetime
import pandas as pd
import itertools
import random
import math
import xarray as xr
from scipy import interpolate
import numpy.ma as ma
from operator import attrgetter

ufiles = '2000_shelftmb_grid_U.nc' 
vfiles = '2000_shelftmb_grid_V.nc' 
mesh_mask = 'coordinates.nc'

filenames = {'U': {'lon': mesh_mask, 'lat': mesh_mask, 'depth': ufiles, 'data': ufiles},
             'V': {'lon': mesh_mask, 'lat': mesh_mask, 'depth': ufiles, 'data': vfiles}}
variables = {'U': 'vozocrtx',
             'V': 'vomecrty'}
dimensions = {'U': {'lon': 'glamf', 'lat': 'gphif', 'depth': 'depthu', 'time': 'time_counter'},
              'V': {'lon': 'glamf', 'lat': 'gphif', 'depth': 'depthu', 'time': 'time_counter'}}
fieldset = FieldSet.from_nemo(filenames, variables, dimensions)

ufields = xr.open_dataset('2000_shelftmb_grid_U.nc')
vfields = xr.open_dataset('2000_shelftmb_grid_V.nc')
landmask = np.logical_or(np.ma.masked_equal(ufields['vozocrtx'][0,0],0.).mask,
                         np.ma.masked_equal(vfields['vomecrty'][0,0],0.).mask)
landmask = landmask.astype('int')
coords = xr.open_dataset(mesh_mask, decode_times=False)
landID = landmask
fieldset.add_field(Field('landmask', landID, lon=fieldset.U.grid.lon, lat=fieldset.U.grid.lat, mesh='spherical'))

lons = fieldset.U.lon
lats = fieldset.U.lat
fieldmesh_x, fieldmesh_y = np.meshgrid(lons, lats)
def Unbeaching_Field(landmask, fieldmesh_x, fieldmesh_y):
    oceancells = np.where(landmask == 0)
    landcells = np.where(landmask == 1)
    vectorfield_x = np.zeros(fieldmesh_x.shape)
    vectorfield_y = np.zeros(fieldmesh_y.shape)
    for i1 in range (len(landcells[1])):
        lon_coast = fieldmesh_x[landcells[0][i1], landcells[1][i1]]
        lat_coast = fieldmesh_y[landcells[0][i1], landcells[1][i1]]
        dist_lon = (lon_coast - fieldmesh_x[oceancells[0], oceancells[1]])
        dist_lat = (lat_coast - fieldmesh_y[oceancells[0], oceancells[1]])
        dist_to_ocean = np.sqrt(np.power(dist_lon, 2) + np.power(dist_lat, 2))
        min_dist = np.min(dist_to_ocean)
        i_min_dist = np.where(dist_to_ocean == min_dist)
        if len(i_min_dist[0]) == 1:
            lon_ocean = fieldmesh_x[oceancells[0][i_min_dist], oceancells[1][i_min_dist]]
            lat_ocean = fieldmesh_y[oceancells[0][i_min_dist], oceancells[1][i_min_dist]]
            vectorfield_x[landcells[0][i1], landcells[1][i1]] = (lon_ocean - lon_coast) / np.sqrt((lon_ocean - lon_coast)**2 + (lat_ocean - lat_coast)**2)
            vectorfield_y[landcells[0][i1], landcells[1][i1]] = (lat_ocean - lat_coast) / np.sqrt((lon_ocean - lon_coast)**2 + (lat_ocean - lat_coast)**2)
        elif len(i_min_dist[0]) > 1:
            lon_ocean = np.mean(fieldmesh_x[oceancells[0][i_min_dist], oceancells[1][i_min_dist]])
            lat_ocean = np.mean(fieldmesh_y[oceancells[0][i_min_dist], oceancells[1][i_min_dist]])
            vectorfield_x[landcells[0][i1], landcells[1][i1]] = (lon_ocean - lon_coast) / np.sqrt((lon_ocean - lon_coast)**2 + (lat_ocean - lat_coast)**2)
            vectorfield_y[landcells[0][i1], landcells[1][i1]] = (lat_ocean - lat_coast) / np.sqrt((lon_ocean - lon_coast)**2 + (lat_ocean - lat_coast)**2)     
    return vectorfield_x, vectorfield_y
landvector_U, landvector_V = Unbeaching_Field(landmask, fieldmesh_x, fieldmesh_y)
U_land = Field('U_land', landvector_U, lon=lons, lat=lats, fieldtype='U', mesh='spherical')
V_land = Field('V_land', landvector_V, lon=lons, lat=lats, fieldtype='V', mesh='spherical')
fieldset.add_field(U_land)
fieldset.add_field(V_land)
vectorfield_unbeaching = VectorField('UV_unbeach', U_land, V_land)
fieldset.add_vector_field(vectorfield_unbeaching)
def Unbeaching(particle, fieldset, time):
    if particle.on_land == 1:
        (u_land, v_land) = fieldset.UV_unbeach[time, particle.depth, particle.lat, particle.lon]
        particle.lon += u_land * particle.dt
        particle.lat += v_land * particle.dt
        particle.on_land = 0

kh_zonal = 100 
kh_meridional = 100 
size2D = (fieldset.U.grid.ydim, fieldset.U.grid.xdim)
fieldset.add_field(Field('Kh_zonal', kh_zonal*np.ones((size2D), dtype=np.float32), lon=fieldset.U.grid.lon, lat=fieldset.U.grid.lat, mesh='spherical'))
fieldset.add_field(Field('Kh_meridional', kh_meridional*np.ones((size2D), dtype=np.float32), lon=fieldset.U.grid.lon, lat=fieldset.U.grid.lat, mesh='spherical'))
fieldset.add_constant('dres', 0.01)

def SampleAge(particle, fieldset, time):
    particle.age += particle.dt 

def Sample_land(particle, fieldset, time):
    particle.on_land = fieldset.landmask[time, particle.depth, particle.lat, particle.lon]

def Particle_death(particle, fieldset, time):
    runtime = 2419200
    if particle.age > runtime:
        particle.delete()

def DeleteParticle(particle, fieldset, time):
    particle.delete()

def Distance(particle, fieldset, time):
    lat_dist = (particle.lat - particle.prev_lat) * 1.11e2
    lon_dist = (particle.lon - particle.prev_lon) * 1.11e2 * math.cos(particle.lat * math.pi/180)
    particle.distance += math.sqrt(math.pow(lon_dist, 2) + math.pow(lat_dist, 2)) 
    particle.prev_lon = particle.lon 
    particle.prev_lat = particle.lat

class CustomParticle(JITParticle):
    on_land = Variable('on_land', to_write=False)
    age = Variable('age', dtype=np.float32, initial=0, to_write=False)
    distance = Variable('distance', dtype=np.float32, initial=0)
    prev_lon = Variable('prev_lon', dtype=np.float32, to_write=False, initial=attrgetter('lon'))
    prev_lat = Variable('prev_lat', dtype=np.float32, to_write=False, initial=attrgetter('lat'))


spawninglocs = pd.read_csv('Spawning_locations_2000_1_deg.csv')
lat = spawninglocs.lat.tolist()
lon = spawninglocs.lon.tolist()
n = 100000
lat = list(itertools.chain.from_iterable(itertools.repeat(x, n) for x in lat))
lon = list(itertools.chain.from_iterable(itertools.repeat(x, n) for x in lon))

start_time = datetime(2000, 7, 18).toordinal()
end_time = datetime(2000, 8, 16).toordinal()
random_ordinal = np.random.randint(start_time, end_time, size=len(lat))
random_start = [datetime.fromordinal(date) for date in random_ordinal]

pset = ParticleSet(fieldset=fieldset, 
                   pclass=CustomParticle,
                   lon=lon,
                   lat=lat,
                   time=random_start)

kernels = pset.Kernel(AdvectionRK4)+ pset.Kernel(Particle_death)+ pset.Kernel(SampleAge)+ pset.Kernel(Sample_land)+ pset.Kernel(Unbeaching)+pset.Kernel(Distance)+pset.Kernel(DiffusionUniformKh)

output_file = pset.ParticleFile(name="2000_Random_100000_passive_2D_1deg_output.nc", outputdt=delta(hours=1))

pset.execute(kernels, runtime=delta(days=43), dt=delta(minutes=5), recovery ={ErrorCode.ErrorOutOfBounds: DeleteParticle, ErrorCode.ErrorInterpolation: DeleteParticle}, output_file = output_file)                             

output_file.close()







