#define MAX_STEPS 100
#define MAX_DIST 100.0
#define MAX_REFLECTION_DEPTH 5
#define FIGURES_AMNT 7
#define LIGHTS_AMNT 4
#define EPS 0.01

#define AMBIENT 0.1

struct Ray 
{
    vec3 pos;
    vec3 dir;
};

struct Hit
{
    bool hit_exist;
    vec3 hit_point;
    float hit_material_reflection;
    vec3 hit_material_color;
};

struct Distance
{
    float value;
    float material_refl;
    vec3 material_color;
};

 
Distance GetDist(vec3 point) {
    Distance res; 
    vec4 sphere = vec4(1,5.1,6,1);
    vec2 ring = vec2(1, 0.5);
    vec3 box = 	vec3(0.8,1.8,0.8);
    Distance dist[FIGURES_AMNT];
    
    
    //vec4 texColor = texture(iChannel0,xy);
    dist[0].value = point.y;
    dist[0].material_refl = 0.0;
    
    vec2 uv = point.xz/(10.0, 100.0);
    vec4 texColor = texture(iChannel0,uv.xy);
    dist[0].material_color = texColor.rgb;
    
    //dist[0].material_color = vec3(0,0.2,0.4);
    
    
    dist[1].value = length(point-sphere.xyz) - sphere.w;
    dist[1].material_refl = 1.;
    dist[1].material_color = vec3(0,0.7,0);
    
  	vec2 q = vec2(length((point-vec3(-3,0, 6)).xz)-ring.x,(point-vec3(-3,1, 6)).y);
    dist[2].value = length(q)-ring.y; 
   	dist[2].material_refl = 1.0;
    dist[2].material_color = vec3(1,0,0);
    
    float r = 0.2;
    dist[3].value = length(max(abs(point - vec3(1,2.0, 6))-box,0.0)) - r;
    dist[3].material_refl = 1.;
    dist[3].material_color = vec3(1, 0, 1);
    
    vec3 box2 = vec3(0.4, 0.4, 0.4);
    vec4 sphere2 = vec4(1,0.4,0,0.5);
    dist[4].value = max(length(max(abs(point-vec3(1,0.4,0))-box2,0.0)), 
                        -(length(point-sphere2.xyz) - sphere2.w));
	dist[4].material_refl = 0.1;
    dist[4].material_color = vec3(0, 0.6, 0.4);
    
    vec3 el = vec3(2,1,1);
    dist[5].value = (length((point-vec3(7, 2, 9))/el) - 1.0) * min(min(el.x,el.y),el.z);
	dist[5].material_refl = .9;
    dist[5].material_color = vec3(1.0, 0.84, 0.0);
     
    vec2 h = vec2(1, 0.2);
    vec2 d = abs(vec2(length((point - vec3(-3, 0.5, 2)).xz),(point - vec3(-3, 0.5, 10)).y)) - h;
    dist[6].value = min(max(d.x,d.y),0.0) + length(max(d,0.0));
    dist[6].material_refl = 0.;
    dist[6].material_color = vec3(0, 0.5, 1);
    
   	res = dist[0];
    for (int i = 1; i < FIGURES_AMNT; i++)
        if (dist[i].value < res.value)
            res = dist[i];
	
    return res;
}


vec3 GetNormal(vec3 point, float eps)
{
    vec3 p1 = point + vec3(eps, 0, 0);
    vec3 p2 = point - vec3(eps, 0, 0);
    vec3 p3 = point + vec3(0, eps, 0);
    vec3 p4 = point - vec3(0, eps, 0);
    vec3 p5 = point + vec3(0, 0, eps);
    vec3 p6 = point - vec3(0, 0, eps);
    
    float dx = GetDist(p1).value - GetDist(p2).value;
    float dy = GetDist(p3).value - GetDist(p4).value;
    float dz = GetDist(p5).value - GetDist(p6).value;
    
    return normalize(vec3(dx, dy, dz)/(2.0*eps));
}

        
        	

Hit RayMarch(Ray ray)
{
    Hit hit;
    float dist = 0.0;
    vec3 point;
    Distance cur_dist;
        
    for (int i=0; i < MAX_STEPS; i++) {
        point = ray.pos + dist*ray.dir;
        cur_dist = GetDist(point);
        dist += cur_dist.value;
        if (dist > MAX_DIST || cur_dist.value < EPS)
            break;
    }    
    hit.hit_exist = false;
    if (cur_dist.value < EPS) 
        hit.hit_exist = true;
    
    hit.hit_point = ray.pos + dist*ray.dir;
    hit.hit_material_reflection = cur_dist.material_refl;
	hit.hit_material_color = cur_dist.material_color;          
	return hit;
}

bool visible(vec3 point1, vec3 point2) 
{ 
    Ray ray;
    ray.dir = normalize(point2-point1);
    ray.pos = point1;
    Hit hit = RayMarch(ray);
    if (length(hit.hit_point-point1) < length(point2-point1)) {
        return false;
    }
    return true;
}


Ray GetReflection (Ray orig, Hit hit)
{
    vec3 n = GetNormal(hit.hit_point, EPS);
    Ray ray;
    ray.dir = orig.dir - 2.0*n*dot(orig.dir, n);//dot(n,n); 
    ray.pos = hit.hit_point + n*EPS;
    return ray;
}


vec3 Shade(Hit hit, Ray ray) 
{	
    vec3 point = hit.hit_point;
    vec4 lights[LIGHTS_AMNT];
    lights[0] = vec4(3, 2, 6, 0.05);
    lights[1] = vec4(-5, 2, 6, 0);
    lights[2] = vec4(-2, 4, 4.0*sin(0.5*iTime), 0.8);
    lights[3] = vec4(3, 4, -3, 0.5);
    
    float light_intensity = AMBIENT; 
    vec3 n = GetNormal(point, EPS);
    for (int i = 0; i < LIGHTS_AMNT; i++) {
        
        if (visible(point+n*EPS, lights[i].xyz)) {
        	vec3 l = normalize(lights[i].xyz - point);
        	
            float dif = max(dot(n, l), 0.0);
        	
            Ray r;
            r.dir = -l;
            r.pos = point;
            Ray l_back = GetReflection(r, hit);
            
            float specular =  pow(max(dot(l_back.dir, -ray.dir), 0.0), 32.0);
           	//specular = 0.0;
            light_intensity += (dif + specular*hit.hit_material_reflection)*lights[i].w/pow(length(l), 2.0); 
        }
    }
    
    return light_intensity*hit.hit_material_color;
}


void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
	vec3 color = vec3(0.0, 0.0, 0.0);
    Ray ray;
    
    vec2 uv = (fragCoord - 0.5*iResolution.xy)/iResolution.y;
    vec2 mouse = (iMouse.xy - 0.5*iResolution.xy)/iResolution.y;
    
    ray.pos = vec3(0,3,-6); //ray origin
    ray.dir = normalize(vec3(uv.xy, 1)); //ray direction
    ray.pos += vec3(mouse.xy*5.0, 0);
     
    
    float k = 1.0;
    for (int i=0; i < MAX_REFLECTION_DEPTH; i++) {
        
        Hit hit = RayMarch(ray);
        color += k*Shade(hit, ray);
        
       	if (hit.hit_material_reflection == 0.0)
            break;
            
        ray = GetReflection(ray, hit);
        k*=hit.hit_material_reflection;
    }       
    fragColor = vec4(color, 1.0);
    
}
