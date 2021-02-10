package basics

object Solution {
  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scanLeft(0) { (acc, el) => acc + el }.drop(1)
  }

  // https://leetcode.com/problems/shuffle-the-array/
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    (0 to n - 1).flatMap(i => Array(nums(i), nums(n + i))).toArray
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(as => as.sum).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(
      candies: Array[Int],
      extraCandies: Int
  ): Array[Boolean] = {
    candies.map(c => c + extraCandies).map(c => c >= candies.max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    //val sortedXs = points.map(p => p(0)).sorted
    //sortedXs.zip(sortedXs.tail).map(ps => ps._2 - ps._1).max
    points
      .map(p => p(0))
      .sorted
      .scanLeft[(Option[Int], Option[Int])](None, None)((acc, x1) =>
        acc match {
          case (Some(w), Some(x0)) => if (w > x1 - x0) (Some(w), Some(x1)) else (Some(x1 - x0), Some(x1))
          case (None, Some(x0)) => (Some(x1 - x0), Some(x1))
          case _                => (None, Some(x1))
        }
      )
      .flatMap(_._1)
      .max
  }
}
