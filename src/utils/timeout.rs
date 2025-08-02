use std::future::Future;
use std::time::Duration;

/// Execute a future with optional timeout
pub async fn execute_with_optional_timeout<T, F>(
    future: F,
    timeout_secs: Option<u64>,
) -> Result<T, String>
where
    F: Future<Output = Result<T, String>>,
{
    if let Some(timeout_secs) = timeout_secs {
        let timeout_duration = Duration::from_secs(timeout_secs);

        // Use tokio::time::timeout for async timeout functionality
        match tokio::time::timeout(timeout_duration, future).await {
            Ok(result) => result,
            Err(_) => Err(format!(
                "Execution timed out after {} seconds",
                timeout_secs
            )),
        }
    } else {
        future.await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_execute_with_timeout_success() {
        let future = async { Ok::<String, String>("success".to_string()) };
        let result = execute_with_optional_timeout(future, Some(5)).await;
        assert_eq!(result, Ok("success".to_string()));
    }

    #[tokio::test]
    async fn test_execute_with_timeout_none() {
        let future = async { Ok::<String, String>("success".to_string()) };
        let result = execute_with_optional_timeout(future, None).await;
        assert_eq!(result, Ok("success".to_string()));
    }

    #[tokio::test]
    async fn test_execute_with_timeout_error() {
        let future = async { Err::<String, String>("error".to_string()) };
        let result = execute_with_optional_timeout(future, Some(5)).await;
        assert_eq!(result, Err("error".to_string()));
    }

    #[tokio::test]
    async fn test_execute_with_timeout_actual_timeout() {
        let future = async {
            tokio::time::sleep(Duration::from_secs(2)).await;
            Ok::<String, String>("success".to_string())
        };
        let result = execute_with_optional_timeout(future, Some(1)).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("timed out"));
    }
}
