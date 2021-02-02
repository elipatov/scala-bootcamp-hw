package basics

import scala.math._

object ClassesAndTraits {
  sealed trait Located2D {
    def x: Double
    def y: Double
  }

  sealed trait Bounded2D {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Shape2D extends Located2D with Bounded2D{
    def area: Double
  }

  final case class Square(centerX: Double, centerY: Double, size: Double) extends Shape2D {
    override def area: Double = size * size
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - 0.5 * size
    override def maxX: Double = centerX + 0.5 * size
    override def minY: Double = centerY - 0.5 * size
    override def maxY: Double = centerY + 0.5 * size
  }

  final case class Triangle(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double) extends Shape2D {
    override def area: Double = math.abs((x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) / 2)
    override def x: Double = (x1 + x2 + x3) / 3
    override def y: Double = (y1 + y2 + y3) / 3
    override def minX: Double = math.min(x1, math.min(x2, x3))
    override def maxX: Double = math.max(x1, math.max(x2, x3))
    override def minY: Double = math.min(y1, math.min(y2, y3))
    override def maxY: Double = math.max(y1, math.max(y2, y3))
  }

  sealed trait Located3D extends Located2D {
    def z: Double
  }

  sealed trait Bounded3D extends Bounded2D {
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Shape3D extends Located3D with Bounded3D{
    def surfaceArea: Double
    def volume: Double
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D{
    override def surfaceArea: Double = 0
    override def volume: Double = 0
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
  }

  final case class Sphere(x: Double, y: Double, z: Double, radius: Double) extends Shape3D{
    override def surfaceArea: Double = 4 * math.Pi * radius * radius
    override def volume: Double = 4 * math.Pi * radius * radius * radius / 3
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius
  }

  final case class Cube(x: Double, y: Double, z: Double, size: Double) extends Shape3D{
    override def surfaceArea: Double = 6 * size * size
    override def volume: Double = math.pow(size, 3)
    override def minX: Double = x - 0.5 * size
    override def maxX: Double = x + 0.5 * size
    override def minY: Double = y - 0.5 * size
    override def maxY: Double = y + 0.5 * size
    override def minZ: Double = z - 0.5 * size
    override def maxZ: Double = z + 0.5 * size
  }

  final case class Cuboid(x: Double, y: Double, z: Double, xSize: Double, ySize: Double, zSize: Double) extends Shape3D{
    override def surfaceArea: Double = 2 * xSize * xSize + 2 * ySize * ySize + 2 * zSize * zSize
    override def volume: Double = xSize * ySize * zSize
    override def minX: Double = x - 0.5 * xSize
    override def maxX: Double = x + 0.5 * xSize
    override def minY: Double = y - 0.5 * ySize
    override def maxY: Double = y + 0.5 * ySize
    override def minZ: Double = z - 0.5 * zSize
    override def maxZ: Double = z + 0.5 * zSize
  }

  final case class Triangle3D(p1: Point3D, p2: Point3D, p3: Point3D) extends Shape3D {
    override def surfaceArea: Double = ???
    override def volume: Double = 0
    override def x: Double = (p1.x + p2.x + p3.x) / 3
    override def y: Double = (p1.y + p2.y + p3.y) / 3
    override def z: Double = (p1.z + p2.z + p3.z) / 3
    override def minX: Double = math.min(p1.x, math.min(p2.x, p3.x))
    override def maxX: Double = math.max(p1.x, math.max(p2.x, p3.x))
    override def minY: Double = math.min(p1.y, math.min(p2.y, p3.y))
    override def maxY: Double = math.max(p1.y, math.max(p2.y, p3.y))
    override def minZ: Double = math.min(p1.z, math.min(p2.z, p3.z))
    override def maxZ: Double = math.max(p1.z, math.max(p2.z, p3.z))
  }
}
