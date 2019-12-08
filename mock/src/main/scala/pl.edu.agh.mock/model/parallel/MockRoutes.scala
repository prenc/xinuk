package pl.edu.agh.mock.model.parallel

case class MockRoutes(var routeThroughWorkers: List[Int], var routeToDestination: List[(Int, Int)], var previousWorkerId: Int )
