import { Editor } from "tldraw";

/**
 * Represents a 2D vector.
 */
type Vec2 = {
	/**
	 * The x coordinate of the vector.
	 */
	x: number;
	/**
	 * The y coordinate of the vector.
	 */
	y: number;
};

/**
 * Represents an edge, composed of two 2D vectors.
 */
type Edge = {
	/**
	 * The start point of the edge.
	 */
	start: Vec2;
	/**
	 * The end point of the edge.
	 */
	end: Vec2;
};

/**
 * Represents an intersection, which is the id of the shape that was
 * intersected and the point of intersection.
 */
type Intersection = {
	/**
	 * The id of the shape that was intersected.
	 */
	id: string;
	/**
	 * The point of intersection.
	 */
	point: Vec2;
};

/**
 * The ray-segment intersection type also includes how far along the ray that the
 * intersection is and how far along the segment that the intersection is.
 */
type IntersectionRaySegment = {
	/**
	 * The id of the shape that was intersected.
	 */
	id: string;
	/**
	 * The point of intersection.
	 */
	point: Vec2;
	/**
	 * How far along the ray (0 at the origin or x units away) the
	 * intersection is.
	 */
	t: number;
	/**
	 * How far along the segment (0 at the start, 1 at the end) the
	 * intersection is.
	 */
	u: number;
};

/**
 * Calculates the dot product of two 2D vectors.
 *
 * @param {Vec2} vec1 The first 2D vector.
 * @param {Vec2} vec2 The second 2D vector.
 *
 * @returns {number} Returns the dot product of the two 2D vectors.
 */
const dot = (vec1: Vec2, vec2: Vec2): number => {
	return vec1.x * vec2.x + vec1.y * vec1.y;
};

/**
 * Subtracts one 2D vector from another 2D vector.
 *
 * @param {Vec2} vec1 The 2D vector to subtract from.
 * @param {Vec2} vec2 The 2D vector to subtract.
 *
 * @returns {Vec2} Returns a new 2D vector that is the result of subtracting vec2 from vec1.
 */
const sub = (vec1: Vec2, vec2: Vec2): Vec2 => {
	return { x: vec1.x - vec2.x, y: vec1.y - vec2.y };
};

/**
 * Performs a basic stepping ray-cast.
 *
 * @param {Editor} editor The tldraw editor instance.
 * @param {Vec2} rayOrigin The start point of the ray.
 * @param {Vec2} rayDir The direction of the ray.
 * @param {number} rayLength The length of the ray.
 * @param {number} step The amount to step between each check along the ray.
 *
 * @returns {Intersection[]} Returns an array of intersections, which are the id of the shape that was intersected and the point of intersection.
 */
const raycastStep = (
	editor: Editor,
	rayOrigin: Vec2,
	rayDir: Vec2,
	rayLength: number,
	step: number
): Intersection[] => {
	// The current position of the ray, which starts at the ray origin and
	// moves along the ray direction.
	const rayCurrentPos: Vec2 = { x: rayOrigin.x, y: rayOrigin.y };

	// The amount to step in the x and y directions.
	const stepCalc = {
		x: rayDir.x * step,
		y: rayDir.y * step,
	};

	// The current length of the ray. We keep checking while the current
	// length is less than the total length of the ray.
	let rayLengthCurrent = 0;

	// The intersection points of the ray with the shapes on the page.
	let intersections: Intersection[] = [];

	// const update = async () => {
	// Check and extend the ray while the current length is less than
	// the total length of the ray.
	while (rayLengthCurrent < rayLength) {
		// Extend the ray by the step amount.
		rayCurrentPos.x += stepCalc.x;
		rayCurrentPos.y += stepCalc.y;

		// Get the shapes at the point of the ray, if any exist. We also
		// exclude any shapes that we've already intersected with.
		const shapesAtPoint = editor
			.getShapesAtPoint(
				{
					x: rayCurrentPos.x,
					y: rayCurrentPos.y,
				},
				{
					hitInside: true,
					margin: 10,
				}
			)
			.filter((shape) => intersections.every((i) => i.id !== shape.id));

		// Update the intersections with the shapes at the point of
		// the ray.
		intersections = [
			...intersections,
			...shapesAtPoint.map((shape) => ({
				id: shape.id,
				point: { x: rayCurrentPos.x, y: rayCurrentPos.y },
			})),
		];

		// Update the current length of the ray.
		rayLengthCurrent += Math.sqrt(stepCalc.x ** 2 + stepCalc.y ** 2);
	}

	return intersections;
};

/**
 * Performs advanced parametric ray-segment intersection checks.
 *
 * @param {Editor} editor The tldraw editor instance.
 * @param {Vec2} rayOrigin The start point of the ray.
 * @param {Vec2} rayDir The direction of the ray.
 * @param {number} rayLength The length of the ray.
 *
 * @returns {IntersectionRaySegment[]} Returns an array of intersections, which are the id of the shape that was intersected and the point of intersection.
 */
const raycastRaySegment = (
	editor: Editor,
	rayOrigin: Vec2,
	rayDir: Vec2,
	rayLength: number
): IntersectionRaySegment[] => {
	// Get all shapes on the page.
	const shapes = editor.getCurrentPageShapes();

	// The intersection point of the ray with the shapes on the page.
	let intersections: IntersectionRaySegment[] = [];

	// We do all of our calculates on a shape's edges so we need each pair
	// of vertices for each shape, along with the id of the shape.
	shapes.forEach((shape) => {
		// `getShapeGeometry` returns lots of useful information like the
		// shape's bounds, vertices, and more.
		// Geometry should always be populated, but sometimes I've gotten
		// `cannot read properties of undefined` errors when trying to
		// access properties off geometry so we'll check just in case.
		const geometry = editor.getShapeGeometry(shape.id);
		if (!geometry) return;

		// We're going to check the ray against edges of shapes instead of
		// entire shapes because a shape can be weirdly shaped.
		// For each vertex in the shape, create an edge which is what
		// we'll use to check for intersections with the ray.
		geometry.vertices.forEach((vertex, i) => {
			// If the shape is not a closed shape, like a line, and we're
			// at the last vertex, we don't want to create an edge with
			// the next vertex because it's not connected to anything.
			if (!geometry.isClosed && i === geometry.vertices.length - 1) {
				return;
			}

			// Save the current vertex and then the one after it.
			// If the shape is a closed shape, like a polygon, the
			// last vertex will be connected to the first vertex.
			const nextVertex =
				geometry.vertices[(i + 1) % geometry.vertices.length];

			// Create an edge from the current vertex to the next vertex.
			const edge: Edge = {
				start: { x: shape.x + vertex.x, y: shape.y + vertex.y },
				end: {
					x: shape.x + nextVertex.x,
					y: shape.y + nextVertex.y,
				},
			};

			// First, we perform dot product culling to eliminate the
			// edge if it's facing away from the ray.
			const dotStart = dot(sub(edge.start, rayOrigin), rayDir);
			const dotEnd = dot(sub(edge.end, rayOrigin), rayDir);
			if (dotStart < 0 && dotEnd < 0) {
				return;
			}

			// The segment vector, which is how we move from the start of
			// the edge to the end of the edge.
			const segment = sub(edge.end, edge.start);

			// The Δ (delta) vector, which is how we move from the start
			// of the edge to the ray's origin.
			// This helps us figure out the relative positioning of the
			// ray origin and the start of the edge.
			const delta = sub(rayOrigin, edge.start);

			// We want to solve the equations:
			//
			// Ray: rayOrigin + t * rayDir (where t >= 0, meaning forward along the ray).
			// Segment: edge.start + u * segment (where 0 <= u <= 1, meaning between the start and end of the edge).
			//
			// The determinant helps us see if there's an intersection. If
			// the determinant is zero, or very close to zero, it means
			// the lines are parallel or nearly parallel.
			const det = segment.x * -rayDir.y - segment.y * -rayDir.x;

			// If the absolute value of the determinant is extremely
			// small, we treat the lines as parallel.
			// In that case, there's either no intersection or infinitely
			// many if they overlap. We'll treat it as no intersection.
			// `1e-12` is arbitrary and can be adjusted. We just need a
			// small number close to zero to compare against.
			if (Math.abs(det) < 1e-12) {
				return;
			}

			// If the lines are not parallel, solve for u and t:
			//
			// `u` tells us where along the segment edge.start->edge.end we intersect.
			// `t` tells us where along the ray rayOrigin->(rayOrigin + rayDirection) we intersect.
			//
			// The formulas come from rearranging rayOrigin + t * rayDir = edge.start + u * segment and solving.
			const u = (-delta.x * rayDir.y + delta.y * rayDir.x) / det;
			const t = (segment.x * delta.y - segment.y * delta.x) / det;

			// We only consider the line segment from the start to the end
			// so `u` must be between 0 and 1.
			// If `u < 0` or `u > 1`, the intersection point is not actually on the segment.
			if (u < 0 || u > 1) {
				return;
			}

			// We also only consider the ray going forward from the
			// origin, and within the maximum length. So `t` must be
			// between 0 and the ray of the length.
			// If `t < 0`, it means the intersection is behind the origin along the ray (this shouldn't happen since we culled those edges previously).
			// If `t > rayLength`, it means the intersection is beyond the ray's allowed length.
			if (t < 0 || t > rayLength) {
				return;
			}

			// If both checks pass, we have an intersection.
			// We can find the exact (x, y) intersection point by
			// plugging `t` back into:
			// Intersection = rayOrigin + t * rayDir
			const intersect: Vec2 = {
				x: edge.start.x + u * segment.x,
				y: edge.start.y + u * segment.y,
			};

			// Save the intersection point and the id of the shape that
			// was intersected.
			intersections = [
				...intersections,
				{
					id: shape.id,
					point: intersect,
					t,
					u,
				},
			];
		});
	});

	return intersections;
};
